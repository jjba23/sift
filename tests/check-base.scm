;; sift --- the walk over code positions
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A form is a node in a position where code is evaluated, and four places are
;; data instead: inside a quote, the datum of a `case` clause, a parameter list
;; and a vector literal.  That question is answered twice -- `walk-forms'
;; carries the answer down the tree, and `code-position?' recomputes it from an
;; ancestor list -- because the first is what a checker wants and the second is
;; what a checker that already has ancestors can ask.
;;
;; Two answers to one question drift unless something compares them, so this
;; suite walks a corpus of everything ambiguous through both and asserts that
;; they name the same nodes, in the same order.  A rule that starts reporting
;; on the inside of a quoted list is exactly the bug this catches.

(define-module (tests check-base)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift check base)
  #:export (spec))

;;; ===== The corpus =====

;; Every snippet is one of the four ways a node stops being code, plus the
;; ways out of them an unquote provides.
(define ambiguous-sources
  '("(quote (if a b))\n"
    "'(if a b)\n"
    "`(a ,(if b c) d)\n"
    "`(a ,@(if b (list c)) d)\n"
    "(case x\n  ((if a b) 1)\n  (else (if a b)))\n"
    "(define (f (if #f)) (if a b))\n"
    "(lambda (x) (lambda (y) y))\n"
    "#(if a b)\n"
    "#u8(1 2 3)\n"
    "(define x #(1 #(2 (if a b))))\n"
    "(let loop ((i 0)) (when (< i 1) (loop (+ i 1))))\n"
    "(define-syntax m (syntax-rules () ((_ a) a)))\n"
    "#'(a b)\n"
    "(syntax-case stx ()\n  ((_ a) #'a))\n"
    "(case x\n  ((a b) 1)\n  (else 2))\n"
    "(defun f (x) \"doc\" (if x y z))\n"
    "(defmacro m (a) `(list ,a))\n"
    "#+foo (if a b)\n"
    "(quasiquote (a (unquote (if x y))))\n"
    "; a comment\n(if a b)\n"))

(define (nodes-in-code-source ast)
  "Return the nodes of @var{ast} that @code{walk-forms} calls code, in order."
  (let ((seen '()))
    (walk-forms (lambda (node ancestors) (set! seen (cons node seen))) ast)
    (reverse seen)))

(define (nodes-with-ancestors ast)
  "Return the nodes of @var{ast} that @code{code-position?} calls code."
  (let ((seen '()))
    (walk-with-ancestors
     (lambda (node ancestors)
       (when (and (not (node-trivia? node))
               (code-position? node ancestors))
         (set! seen (cons node seen))))
     ast)
    (reverse seen)))

(define (read-ast source)
  (call-with-values
    (lambda () (read-source #:source source #:dialect 'scheme))
    (lambda (ast errors) ast)))

(define (same-nodes? expected actual)
  "Return @code{#t} when two lists hold the same nodes, in the same order."
  (and (= (length expected) (length actual))
    (every (lambda (a b) (eq? a b)) expected actual)))

(define (describe nodes source)
  "Return the source text of @var{nodes}, for a message that can be read."
  (map (lambda (node)
         (if (node-list? node)
           (string-append "(" (if (node-head-name node)
                                (symbol->string (node-head-name node))
                                "?")
                          " ...)")
           (let ((start (node-index node))
                 (end (node-end-index node)))
             (if (<= end (string-length source))
               (substring source start end)
               "?"))))
       nodes))

;;; ===== The suite =====

(define (spec)
  (suite "walk: where code is evaluated"

    (test "a quoted form holds data, not code"
      ;; the call, its operator, and the quote mark: nothing the quote holds
      (let* ((source "(car '(if a b))\n")
             (ast (read-ast source))
             (nodes (nodes-in-code-source ast)))
        (assert-equal #:expect '(#t #f #f)
          #:got (map node-list? nodes)
          #:name "only the call is a form")
        (assert-equal #:expect 3 #:got (length nodes)
          #:name "the call, `car`, and the quote mark")))

    (test "`walk-forms' and `code-position?' agree on every ambiguous source"
      (for-each
       (lambda (source)
         (let ((ast (read-ast source)))
           (assert-true (same-nodes? (nodes-with-ancestors ast)
                                     (nodes-in-code-source ast))
             #:name (format #f "~s agrees" source))))
       ambiguous-sources))

    (test "a `case` datum is data, and its clauses are code"
      (let* ((source "(case x ((a b) (f a)) (else (g b)))\n")
             (ast (read-ast source))
             (names (filter-map (lambda (node)
                                  (and (node-list? node) (node-head-name node)))
                                (nodes-in-code-source ast))))
        (assert-equal #:expect '(case f else g)
          #:got names
          #:name "the `case`, and the calls in the clause bodies")))

    (test "a parameter list holds parameters, not forms"
      (let* ((source "(define (f (g #f)) (g 1))\n")
             (ast (read-ast source))
             (names (filter-map (lambda (node)
                                  (and (node-list? node) (node-head-name node)))
                                (nodes-in-code-source ast))))
        (assert-equal #:expect '(define g)
          #:got names
          #:name "the definition and the call, not the default's `g`")))

    (test "nothing inside a vector literal is code"
      (let* ((source "(begin #(if a b) #u8(1) (f))\n")
             (ast (read-ast source))
             (names (filter-map (lambda (node)
                                  (and (node-list? node) (node-head-name node)))
                                (nodes-in-code-source ast))))
        (assert-equal #:expect '(begin f)
          #:got names
          #:name "the two calls, and neither literal")))

    (test "an unquote is code again"
      (let* ((source "`(a ,(if b c) ,@(list d))\n")
             (ast (read-ast source))
             (names (filter-map (lambda (node)
                                  (and (node-list? node) (node-head-name node)))
                                (nodes-in-code-source ast))))
        ;; this reader spells a quasiquote as a prefix, so the only lists in
        ;; code position there are the two forms the unquotes put back
        (assert-equal #:expect '(if list)
          #:got names
          #:name "the two forms inside unquotes, and nothing else")))))
