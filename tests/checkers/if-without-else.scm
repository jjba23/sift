;; sift --- checker: if-without-else
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; An `if` with no else branch is a `when` spelled the long way.  The fix
;; rewrites the operator, which is the whole change.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers if-without-else)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: if-without-else"

    (test "reports an if with two arguments"
      (assert-warnings #:source "(if a b)\n" #:checker 'if-without-else
                       #:expect '((1 2))))

    (test "accepts an if with an else branch"
      (assert-warnings #:source "(if a b c)\n" #:checker 'if-without-else
                       #:expect '()))

    (test "a vector literal is data, not a form"
      (assert-warnings #:source "(define v #(if a b))\n"
                       #:checker 'if-without-else
                       #:expect '()))

    (test "a `match` pattern is data, and its evaluated parts are not"
      ;; a pattern names a shape; rewriting the `if` inside it would change the
      ;; macro's pattern rather than the program's control flow
      (assert-warnings #:source "(match x [(_ (if a b)) 1])\n"
                       #:checker 'if-without-else #:expect '())
      (assert-warnings #:source "(match-lambda [(_ (if a b)) 1])\n"
                       #:checker 'if-without-else #:expect '())
      (assert-warnings #:source "(syntax-case stx ()\n  [(_ (if a b)) 1])\n"
                       #:checker 'if-without-else #:expect '())
      ;; `?` and `app` evaluate their first argument, so code comes back
      (assert-warnings #:source "(match x [(? (lambda (v) (if v 1)) y) 1])\n"
                       #:checker 'if-without-else #:expect '((1 27)))
      (assert-warnings #:source "(match x [(app (lambda (v) (if v 1)) y) 1])\n"
                       #:checker 'if-without-else #:expect '((1 29)))
      ;; `#:when` guards and `case` keys are ordinary expressions
      (assert-warnings #:source "(match x [y #:when (if y 1) 3])\n"
                       #:checker 'if-without-else #:expect '((1 21)))
      (assert-warnings #:source "(case (if a b) ((1) 2))\n"
                       #:checker 'if-without-else #:expect '((1 8))))

    (test "the subjects of a `match*` are code, and its clauses are patterns"
      (assert-warnings #:source "(match* ((if a b) (g)) [(_ _) 1])\n"
                       #:checker 'if-without-else #:expect '((1 11)))
      (assert-warnings #:source "(match* ((f) (g)) [((if a b) _) 1])\n"
                       #:checker 'if-without-else #:expect '()))

    (test "a `case` datum is data all the way down"
      (assert-warnings #:source "(case x ((a (if b c)) 1) (else 2))\n"
                       #:checker 'if-without-else #:expect '()))

    (test "a quasiquoted or quasisyntaxed template is data"
      (assert-warnings #:source "(define x `(a (if b c)))\n"
                       #:checker 'if-without-else
                       #:expect '())
      (assert-warnings #:source "(define x #`(if a b))\n"
                       #:checker 'if-without-else
                       #:expect '()))

    (test "what an unquote puts back is code again"
      (assert-warnings #:source "(define x `(a ,(if b c)))\n"
                       #:checker 'if-without-else
                       #:expect '((1 17)))
      (assert-warnings #:source "(define x #`(list #,(if a b)))\n"
                       #:checker 'if-without-else
                       #:expect '((1 22)))
      (assert-warnings #:source "(define x `#(1 ,(if a b)))\n"
                       #:checker 'if-without-else
                       #:expect '((1 18))))

    (test "the fix rewrites it as when"
      (assert-fix #:source "(if a b)\n" #:checker 'if-without-else
                  #:expected "(when a b)\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "(if a b)\n" #:checker 'if-without-else))))
