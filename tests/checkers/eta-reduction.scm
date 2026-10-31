;; sift --- checker: eta-reduction
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `(lambda (x) (f x))` is `f`.  The rule is about the shape and nothing else,
;; so what it must not do is as important as what it must: an argument that is
;; not the formal, a second argument, a special form in the operator, and a
;; lambda with more than one formal all leave the lambda alone.

(define-module (tests checkers eta-reduction)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: eta-reduction"

    (test "reports a lambda that only passes its argument on"
      (assert-warnings #:source "(lambda (x) (f x))\n" #:checker 'eta-reduction
                       #:expect '((1 1))))

    (test "the fix names the procedure itself"
      (assert-fix #:source "(lambda (x) (f x))\n" #:checker 'eta-reduction
                  #:expected "f\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "(lambda (x) (f x))\n"
                               #:checker 'eta-reduction))

    (test "accepts a call that passes more than the argument"
      (assert-warnings #:source "(lambda (x) (f x y))\n" #:checker 'eta-reduction
                       #:expect '()))

    (test "accepts a call that passes something else"
      (assert-warnings #:source "(lambda (x) (f 1))\n" #:checker 'eta-reduction
                       #:expect '()))

    (test "accepts a body that is more than the call"
      (assert-warnings #:source "(lambda (x) (f x) (g x))\n"
                       #:checker 'eta-reduction
                       #:expect '()))

    (test "accepts a lambda with two formals"
      (assert-warnings #:source "(lambda (x y) (f x))\n" #:checker 'eta-reduction
                       #:expect '()))

    (test "accepts a rest argument"
      (assert-warnings #:source "(lambda args (f args))\n" #:checker 'eta-reduction
                       #:expect '()))

    (test "never rewrites a special form into a value"
      (assert-warnings #:source "(lambda (x) (if x))\n" #:checker 'eta-reduction
                       #:expect '()))

    (test "an Emacs Lisp lambda is not a Scheme function designator"
      (assert-warnings #:source "(lambda (x) (f x))\n" #:checker 'eta-reduction
                       #:dialect 'elisp
                       #:expect '()))))
