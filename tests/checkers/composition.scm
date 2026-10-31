;; sift --- checker: composition
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `(lambda (x) (f (g x)))` is `(compose f g)` in Guile, and the rule says so
;; because a pipeline that is written as a pipeline is read as one.  It is
;; Scheme only: `compose` is Guile's, and suggesting it to a Common Lisp
;; project would be suggesting a library it may not have.

(define-module (tests checkers composition)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: composition"

    (test "reports a lambda whose body is one call around another"
      (assert-warnings #:source "(lambda (x) (f (g x)))\n" #:checker 'composition
                       #:expect '((1 1))))

    (test "the fix names the composition"
      (assert-fix #:source "(lambda (x) (f (g x)))\n" #:checker 'composition
                  #:expected "(compose f g)\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "(lambda (x) (f (g x)))\n"
                               #:checker 'composition))

    (test "accepts an inner call that takes more than the argument"
      (assert-warnings #:source "(lambda (x) (f (g x y)))\n"
                       #:checker 'composition
                       #:expect '()))

    (test "accepts a body that is not a nested call"
      (assert-warnings #:source "(lambda (x) (+ 1 x))\n" #:checker 'composition
                       #:expect '()))

    (test "accepts a lambda that takes two arguments"
      (assert-warnings #:source "(lambda (x y) (f (g x)))\n"
                       #:checker 'composition
                       #:expect '()))

    (test "does not suggest Guile's compose to Common Lisp"
      (assert-warnings #:source "(lambda (x) (f (g x)))\n" #:checker 'composition
                       #:dialect 'cl
                       #:expect '()))))
