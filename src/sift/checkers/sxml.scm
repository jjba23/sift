;; sift --- SXML
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; SXML is Lisp used as markup: `(element (@ (attribute "value")) child ...)'.
;; A formatter does not need to know that -- an element is a list and formats
;; as one, and the attribute list, being the first argument, lines up under
;; itself the way every call does -- but a linter can say the three things the
;; shape itself cannot:
;;
;;   the attribute list belongs first, and there is one of it
;;   a child is an element, a string or a number, not a list of them
;;   an element with attributes and no children is a tag that will not close
;;
;; All three are about documents, so none of them fires on ordinary code.  The
;; test for "is this a document" is the attribute list itself: a list whose
;; head is a symbol and that holds an `(@ ...)' child is an element, and
;; nothing else is.  That is the one piece of evidence a file carries, and it
;; is enough -- a function called `div' that takes an `@' list is not something
;; anyone writes by accident.

(define-module (sift checkers sxml)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift check base)
  #:export (sxml-checkers
            sxml-attributes-checker
            sxml-child-checker
            sxml-empty-element-checker))

;;; ===== Reading an element =====

(define (attribute-list? node)
  "Return @code{#t} when @var{node} is an SXML attribute list, @code{(@ ...)}."
  (and (node-list? node) (eq? (node-head-name node) '@)))

(define (element? node)
  "Return @code{#t} when @var{node} looks like an SXML element.

An element is a list whose head is a symbol, and the evidence is the attribute
list: `(div (@ (class \"x\")) ...)'.  A list with no attribute list could be
anything, and is left alone."
  (and (node-list? node)
    (let ((head (node-head node)))
      (and (node-atom? head)
        (eq? (node-atom-type head) 'symbol)
        (any attribute-list? (node-elements node))))))

(define (children-of node)
  "Return the children of an element: its elements, with trivia dropped."
  (cdr (node-elements node)))

;;; ===== The attribute list =====

(define (sxml-attributes-warnings ast source dialect)
  (let ((warnings '()))
    (define (report node message)
      (set! warnings
            (cons (make-lint-warning 'sxml-attributes 'warning message
                                     (node-start node))
                  warnings)))
    (for-each-node
     (lambda (node)
       (when (element? node)
         (let* ((children (children-of node))
                (lists (filter attribute-list? children))
                (first (and (pair? children) (car children))))
           (cond
             ;; more than one: SXML has one attribute list per element
             [(> (length lists) 1)
              (report (cadr lists)
                      "an element has one attribute list; merge these into one `(@ ...)'")]
             ;; present, but not first: the attributes are read before anything
             [(and (pair? lists) (not (eq? first (car lists))))
              (report (car lists)
                      "the attribute list belongs right after the element name")]
             ;; empty: `(@)' says nothing
             [(and (pair? lists) (null? (children-of (car lists))))
              (report (car lists)
                      "an empty attribute list adds nothing; remove it")]))))
     ast)
    (reverse warnings)))

;;; ===== The element with nothing in it =====
;;;
;;; `(div (@ (class "card")))' is markup with an attribute list and no
;;; children, and SXML serializes that as a self-closing tag: `<div
;;; class="card"/>'.  In XML that is a complete element.  In HTML it is not --
;;; the parser does not accept the self-closing form for anything that is not
;;; a void element, so the `<div/>' is read as an open `<div>' and swallows
;;; everything after it.  One missing child is therefore a page whose layout
;;; collapses, and the fix is the one a person would write: `(div (@ (class
;;; "card")) "")', an element with an empty text child, which serializes as
;;; `<div class="card"></div>'.
;;;
;;; The rule reports the element whose *only* child is a non-empty attribute
;;; list, so it says one thing per element and leaves the two shape rules to
;;; say theirs first: an element with no attributes at all is ordinary code,
;;; and an element whose attribute list is empty or repeated has a shape
;;; problem that `sxml-attributes' reports already.

(define (empty-element-fix close)
  "Return a fix that writes an empty string child before @var{close}, or #f.

The replacement is the element's own text with ` \"\"' before its closing
delimiter, so a fix never re-indents markup the author laid out: the
formatter is the one that decides where the child goes.

No fix is offered for an element the reader could not close -- there is no
delimiter to write the child before, and a fix would have to guess at where
the form ends."
  (and (string? close)
    (not (string-null? close))
    (lambda (source start end)
      (let* ((text (substring source start end))
             (size (string-length text)))
        (if (and (< (string-length close) size) (string-suffix? close text))
          (string-append (substring text 0 (- size (string-length close)))
                         " \"\"" close)
          text)))))

(define (sxml-empty-element-warnings ast source dialect)
  (let ((warnings '())
        (message (string-append
                  "an element has attributes and no children;"
                  " a markup element that is empty says so with `\"\"'")))
    (for-each-node
     (lambda (node)
       (when (element? node)
         (let ((children (children-of node)))
           ;; one child, and it is the element's attributes: nothing else is
           ;; written between the name and the closing delimiter
           (when (and (pair? children)
                   (null? (cdr children))
                   (attribute-list? (car children))
                   (pair? (children-of (car children))))
             (set! warnings
                   (cons (make-lint-warning/range
                          'sxml-empty-element 'warning
                          message
                          (node-start node)
                          (node-index node) (node-end-index node)
                          (empty-element-fix (node-close node)))
                         warnings))))))
     ast)
    (reverse warnings)))

;;; ===== The children =====

(define (sxml-child-warnings ast source dialect)
  (let ((warnings '()))
    (define (report node message)
      (set! warnings
            (cons (make-lint-warning 'sxml-child 'warning message (node-start node))
                  warnings)))
    (for-each-node
     (lambda (node)
       (when (element? node)
         (for-each
          (lambda (child)
            (cond
              ;; a list of elements where one element belongs
              [(and (node-list? child)
                 (let ((head (node-head child)))
                   (and head (node-list? head))))
               (report child
                       "a child is an element, a string or a number; this is a list of them")]
              ;; a list whose head is not a name -- `(('p "x"))'
              [(and (node-list? child) (not (node-head child)))
               (report child "this child has no element name")]
              [else (values)]))
          (filter (lambda (child) (not (attribute-list? child)))
                  (children-of node)))))
     ast)
    (reverse warnings)))

;;; ===== The checkers =====

(define sxml-attributes-checker
  (checker 'sxml-attributes
           "An element's `(@ ...)` list is missing, repeated, empty, or not the first child"
           '(scheme)
           sxml-attributes-warnings
           #:severity 'warning
           #:example '((input . "(div (p \"a\") (@ (class \"x\")))")
                       (fix . "no automatic fix; markup is moved by the author, not by a formatter"))))

(define sxml-child-checker
  (checker 'sxml-child
           "A child of an element that is a list of children rather than a child"
           '(scheme)
           sxml-child-warnings
           #:severity 'warning
           #:example '((input . "(ul (((@ (class \"x\"))) (li \"a\")))")
                       (fix . "no automatic fix; the missing element name is the author's to supply"))))

(define sxml-empty-element-checker
  (checker 'sxml-empty-element
           "An element with attributes and no children, which serializes as a self-closing tag"
           '(scheme)
           sxml-empty-element-warnings
           #:severity 'warning
           #:example '((input . "(div (@ (class \"card\")))")
                       (fix . "(div (@ (class \"card\")) \"\")"))))

;;; ===== The list =====

(define sxml-checkers
  (list sxml-attributes-checker
        sxml-child-checker
        sxml-empty-element-checker))
