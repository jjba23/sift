;; sift --- checker: letrec-init-reference
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; R7RS says an initializer of `letrec' may not refer to the value of any of
;; the variables the form is binding.  All-lambda initializers satisfy that
;; automatically; an initializer that is not a lambda and mentions a binding is
;; a `letrec*' written with the wrong name.

(define-module (tests checkers letrec-init-reference)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: letrec-init-reference"

    (test "reports an initializer that refers to a binding"
      (assert-warnings #:source "(letrec ((a 1) (b (+ a 1))) b)\n"
                       #:checker 'letrec-init-reference
                       #:expect '((1 19))))

    (test "accepts mutually recursive procedures"
      (assert-warnings
       #:source "(letrec ((odd? (lambda (n) (even? n)))\n         (even? (lambda (n) (odd? n))))\n  (even? 2))\n"
       #:checker 'letrec-init-reference
       #:expect '()))

    (test "accepts initializers that mention nothing the form binds"
      (assert-warnings #:source "(letrec ((a 1) (b 2)) (+ a b))\n"
                       #:checker 'letrec-init-reference
                       #:expect '()))

    (test "accepts a self-reference inside a lambda"
      (assert-warnings #:source "(letrec ((loop (lambda (n) (loop n)))) (loop 1))\n"
                       #:checker 'letrec-init-reference
                       #:expect '()))))
