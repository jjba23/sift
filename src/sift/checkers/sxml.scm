;; sift --- SXML
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; SXML is Lisp used as markup: `(element (@ (attribute "value")) child ...)'.
;; A formatter does not need to know that -- an element is a list and formats
;; as one, and the attribute list, being the first argument, lines up under
;; itself the way every call does -- but a linter can say the two things the
;; shape itself cannot:
;;
;;   the attribute list belongs first, and there is one of it
;;   a child is an element, a string or a number, not a list of them
;;
;; Both are about documents, so neither fires on ordinary code.  The test for
;; "is this a document" is the attribute list itself: a list whose head is a
;; symbol and that holds an `(@ ...)' child is an element, and nothing else is.
;; That is the one piece of evidence a file carries, and it is enough -- a
;; function called `div' that takes an `@' list is not something anyone writes
;; by accident.

(define-module (sift checkers sxml)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift check base)
  #:export (sxml-checkers
            sxml-attributes-checker
            sxml-child-checker))

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

;;; ===== The list =====

(define sxml-checkers
  (list sxml-attributes-checker
        sxml-child-checker))
