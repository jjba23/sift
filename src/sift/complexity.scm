;; sift --- how much a definition asks of a reader
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Three numbers describe one definition, because each answers a different
;; question and no single one is enough:
;;
;;   score   cognitive complexity: every decision costs one, and a decision
;;           nested inside another costs one more.  This is the number that
;;           says "this takes effort to read".
;;   cycles  cyclomatic complexity: one plus the number of decisions.  This is
;;           the number that says "how many paths a test would have to cover".
;;   lines   the length of the definition.  This is the number that says "how
;;           far the reader has to scroll to see the whole thing".
;;
;; Plus the deepest nesting of decisions, which is what `nesting-depth' in
;; the linter reports one form at a time.
;;
;; The measurement lives here rather than in any one of the callers because
;; `maak complexity' prints the ranking and two rules report a definition one
;; at a time; a report and a rule that computed the numbers separately would
;; disagree about the same function, and the reader would have no way to tell
;; which one was right.
;;
;; It reads the tree sift's reader produced, so a comment, a string, a datum
;; comment and a quoted datum are never mistaken for code -- an `(if ...)' in a
;; docstring or in a pattern is not a branch.

(define-module (sift complexity)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (sift records)
  #:export (complexity-of
            complexity?
            complexity-score
            complexity-cycles
            complexity-depth
            complexity-lines
            complexity-nodes
            complexity-line
            complexity-name
            complexity-definition?))

;;; ===== What counts as a decision =====

;;; A form that makes the reader hold two or more possibilities at once.  In
;;; a Lisp this is not the same list as the one a brace language would have:
;;; `(let ((x 1)) ...)' is a scope, not a branch, but every `let' clause in a
;;; `cond' is, and `and' short-circuits.  The list below is the union of the
;;; four dialects, because one measurement over a mixed tree is more useful
;;; than four measurements.
(define decision-heads
  '(if when unless cond case case-lambda and or
     let let* letrec letrec* let-values let*-values
     do loop dolist dotimes while
     guard condition-case ignore-errors unwind-protect
     cond-expand typecase etypecase handler-case restart-case
     catch throw))

;;; A `let' is a decision only when it has clauses that choose: a plain
;;; `let' binds, and binding is not branching.  It stays in the list above
;;; because a `cond' clause body usually begins with one, and because the
;;; cost of counting a few scopes is lower than the cost of missing a branch
;;; in a `case'.

(define definition-heads
  '(define define* define-public define-record-type define-syntax
     define-syntax-rule define-macro define-inlinable define-values
     define-constant define-primitive define-option-constant
     lambda define-lambda
     defun defmacro defsubst defcustom defvar defconst defalias
     cl-defun cl-defmacro cl-defgeneric cl-defmethod
     defmethod defgeneric defstruct defclass deftype
     define-derived-mode define-minor-mode define-globalized-minor-mode))

(define (head-symbol node)
  "Return the symbol in head position of @var{node}, or @code{#f}."
  (and (node-list? node) (node-head-name node)))

(define (decision? node)
  (memq (head-symbol node) decision-heads))

(define (definition-head? node)
  (memq (head-symbol node) definition-heads))

(define (quoted-datum? node)
  "Return @code{#t} when @var{node} is `'' applied to what follows."
  (and (node-prefix? node) (equal? (node-token node) "'")))

;;; ===== The measurement =====

(define-record-type <complexity>
  (make-complexity score cycles depth lines nodes line name)
  complexity?
  (score complexity-score)
  (cycles complexity-cycles)
  (depth complexity-depth)
  (lines complexity-lines)
  (nodes complexity-nodes)
  (line complexity-line)
  (name complexity-name))

(define (definition-name node)
  "Return the name a definition form defines, as a string.

`(define (f x) ...)' and `(define f ...)' both answer \"f\"; a form that
names nothing usable falls back to its head, so every line of a report can
be found in the file."
  (let* ((elements (node-elements node))
         (second (and (pair? (cdr elements)) (cadr elements)))
         (name (cond
                 [(node-list? second)
                  (let ((inner (node-head-name second)))
                    (and inner (symbol->string inner)))]
                 [(node-atom? second) (node-value second)]
                 [else #f])))
    (or name
      (let ((head (head-symbol node)))
        (if head (symbol->string head) "?")))))

(define (complexity-of node)
  "Return the measurement of @var{node}, a definition form.

A definition nested inside @var{node} -- a closure returned from a factory,
a local helper -- is skipped rather than counted twice: it is its own unit
of reading, and folding it into its parent would make a factory look as
costly as the function it builds.  A quoted datum is skipped for the same
reason a string is: it is data the program carries, not a branch it takes."
  (let ((decisions 0)
        (score 0)
        (deepest 0)
        (data 0))
    (define (walk inner nesting)
      (unless (or (node-trivia? inner) (node-commented-out? inner))
        (set! data (+ data 1))
        (let ((branching (and (decision? inner) #t))
              (next (if (decision? inner) (+ nesting 1) nesting)))
          (when branching
            (set! decisions (+ decisions 1))
            (set! score (+ score 1 nesting))
            (set! deepest (max deepest (+ nesting 1))))
          (cond
            [(quoted-datum? inner) #f]
            [(node-list? inner)
             (for-each
              (lambda (child)
                (unless (definition-head? child) (walk child next)))
              (node-elements inner))]
            [(node-prefix? inner)
             (let ((child (node-child inner)))
               (when (and child (not (node-datum-comment? inner)))
                 (walk child next)))]
            [(node-conditional? inner)
             (for-each (lambda (part) (when part (walk part next)))
                       (list (node-feature inner) (node-guarded-form inner)))]))))
    (walk node 0)
    (make-complexity score
                     (+ decisions 1)
                     deepest
                     (+ 1 (- (source-location-line (node-end node))
                             (source-location-line (node-start node))))
                     data
                     (source-location-line (node-start node))
                     (definition-name node))))

(define (complexity-definition? node)
  "Return @code{#t} when @var{node} is a form that defines a name."
  (and (node-list? node) (definition-head? node) #t))
