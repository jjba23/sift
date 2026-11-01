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
