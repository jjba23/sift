;; sift --- checker: let-sibling
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `(let ((a (f)) (b (g a))) ...)' reads as if `b' could see `a', and it
;; cannot: the initializers of a `let' are evaluated in the enclosing scope.
;; The fix renames the form to the one that binds in order.

(define-module (tests checkers let-sibling)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: let-sibling"

    (test "reports an initializer that reaches for a sibling"
      (assert-warnings #:source "(let ((a (f)) (b (g a))) b)\n"
                       #:checker 'let-sibling
                       #:expect '((1 18))))

    (test "the fix says let*"
      (assert-fix #:source "(let ((a (f)) (b (g a))) b)\n"
                  #:checker 'let-sibling
                  #:expected "(let* ((a (f)) (b (g a))) b)\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "(let ((a (f)) (b (g a))) b)\n"
                               #:checker 'let-sibling))

    (test "accepts independent initializers"
      (assert-warnings #:source "(let ((a (f)) (b (g 1))) b)\n"
                       #:checker 'let-sibling
                       #:expect '()))

    (test "accepts a name that only appears under a quote"
      (assert-warnings #:source "(let ((a (f)) (b '(a))) b)\n"
                       #:checker 'let-sibling
                       #:expect '()))

    (test "accepts a mention an inner scope binds again"
      (assert-warnings #:source "(let ((a (f)) (b (let ((a 2)) a))) b)\n"
                       #:checker 'let-sibling
                       #:expect '()))

    (test "reports a reference from a lambda body"
      (assert-warnings #:source "(let ((a (f)) (b (lambda () a))) b)\n"
                       #:checker 'let-sibling
                       #:expect '((1 18))))))
