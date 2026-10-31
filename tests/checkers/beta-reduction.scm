;; sift --- checker: beta-reduction
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; `((lambda (x) body) value)` is `(let ((x value)) body)`: the value is
;; evaluated once, before the body, exactly as the call evaluates it.  The
;; rule only counts the shape as reducible when there is one formal and one
;; argument, because a `let` cannot say what a second argument would mean.

(define-module (tests checkers beta-reduction)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: beta-reduction"

    (test "reports a lambda applied in place"
      (assert-warnings #:source "((lambda (x) (f x)) (g))\n"
        #:checker 'beta-reduction
        #:expect '((1 1))))

    (test "the fix writes the let"
      (assert-fix #:source "((lambda (x) (f x)) (g))\n" #:checker 'beta-reduction
        #:expected "(let ((x (g))) (f x))\n"))

    (test "a multi-form body becomes a multi-form let"
      (assert-fix #:source "((lambda (x) (f x) (g x)) (h))\n"
        #:checker 'beta-reduction
        #:expected "(let ((x (h))) (f x) (g x))\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "((lambda (x) (f x)) (g))\n"
        #:checker 'beta-reduction))

    (test "accepts a lambda applied to two arguments"
      (assert-warnings #:source "((lambda (x y) (f x y)) (g) (h))\n"
        #:checker 'beta-reduction
        #:expect '()))

    (test "accepts a lambda with no body"
      (assert-warnings #:source "((lambda (x)) (g))\n" #:checker 'beta-reduction
        #:expect '()))

    (test "accepts a call to something that is not a lambda"
      (assert-warnings #:source "(f (g))\n" #:checker 'beta-reduction
        #:expect '()))

    (test "a Common Lisp lambda applied in place is a let too"
      (assert-fix #:source "((lambda (x) (f x)) (g))\n" #:checker 'beta-reduction
        #:dialect 'cl
        #:expected "(let ((x (g))) (f x))\n"))))
