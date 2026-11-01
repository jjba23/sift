;; sift --- comment checkers
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A comment in a Lisp file is not written the same way wherever it sits.
;; Riastradh's rules give each position its own number of semicolons --
;; four for a heading, three for a top-level comment, two for a comment
;; inside a form, one for a comment at the end of a line -- and the number
;; is how a reader tells, at a glance, whether the comment is about the
;; next form, about the form it is written in, or about the line it ends.
;;
;; Only the line comments are judged.  A block comment has no semicolons to
;; count, and a comment inside a "#;" datum comment is part of code the
;; reader was told not to read, so neither is reported.
;;
;; The rule has no fix.  Changing the number of semicolons in a comment is
;; deciding what the comment is for, and that is the author's.

(define-module (sift checkers comments)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift check base)
  #:export (comments-checkers
            comment-semicolons-checker))

;;; ===== Walking the comments =====

(define (walk-comments proc ast)
  "Call @var{proc} on every line comment, and on whether it sits in a form.

The checkers that walk data nodes never see a comment: comments are trivia,
and `for-each-node' deliberately passes over them.  A rule about how a
comment is written has to walk the children, trivia included, and it has to
know whether there is a form above the comment -- a comment inside a `let' is
not a top-level comment."
  (define (walk node inside?)
    (unless (node-commented-out? node)
      (cond
        [(node-comment? node) (proc node inside?)]
        [(node-list? node)
         (for-each (lambda (child) (walk child #t)) (node-children node))]
        [(node-prefix? node)
         (let ((child (node-child node)))
           (when child (walk child inside?)))]
        [(node-conditional? node)
         (let ((feature (node-feature node))
               (guarded (node-guarded-form node)))
           (when feature (walk feature inside?))
           (when guarded (walk guarded inside?)))])))
  (for-each (lambda (node) (walk node #f)) ast))

;;; ===== Reading a comment =====

(define (semicolon-count text)
  "Return how many semicolons @var{text} begins with."
  (let loop ((i 0))
    (if (and (< i (string-length text)) (char=? (string-ref text i) #\;))
      (loop (+ i 1))
      i)))

(define (comment-text text)
  "Return @var{text} without its leading semicolons."
  (substring text (semicolon-count text) (string-length text)))

(define (space-after-semicolons? text)
  "Return @code{#t} when a space follows the semicolons of @var{text}."
  (let ((rest (comment-text text)))
    (or (string-null? rest) (char=? (string-ref rest 0) #\space))))

;;; ===== comment-semicolons =====

(define (comment-semicolons-warnings ast source dialect)
  (let ((warnings '()))
    (define (report node message)
      (set! warnings
            (cons (make-lint-warning 'comment-semicolons 'info message
                                     (node-start node))
                  warnings)))
    (walk-comments
     (lambda (node inside?)
       (let ((text (node-value node))
             (semicolons (semicolon-count (node-value node))))
         ;; A margin comment is the one position where the semicolons are a
         ;; gesture rather than a level: it belongs to the line of code it
         ;; ends, and one is enough.
         (unless (node-inline? node)
           (cond
             [(not (space-after-semicolons? text))
              (report node "write a space between the semicolons and the text")]
             [inside?
               (when (not (= semicolons 2))
                 (report node "a comment inside a form takes two semicolons"))]
             [(< semicolons 3)
              (report node "a top-level comment takes three semicolons")]
             [else #f]))))
     ast)
    (reverse warnings)))

(define comment-semicolons-checker
  (checker 'comment-semicolons
           "A comment's semicolon level does not say where it belongs"
           'all
           comment-semicolons-warnings
           #:severity 'info
           #:example '((input . "(define (f x)\n  ;; two semicolons, in a body\n  x)\n;; one semicolon, at top level\n")
                       (fix . "use three at top level, two inside a form, one at the end of a line"))))

;;; ===== The list =====

(define comments-checkers
  (list comment-semicolons-checker))
