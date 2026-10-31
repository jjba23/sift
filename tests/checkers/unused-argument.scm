;; sift -- checker: unused-argument
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A parameter nobody refers to is either a leftover or a promise the
;; procedure does not keep.  It is a suggestion rather than a warning because
;; a callback written to an interface has parameters it does not need, which
;; is why `_' in front of the name is the way to say so.

(define-module (tests checkers unused-argument)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: unused-argument"

    (test "reports a parameter the body never uses"
      (assert-warnings #:source "(define (f x y) (list x))\n"
                       #:checker 'unused-argument #:expect '((1 14))))

    (test "accepts a parameter that is used"
      (assert-warnings #:source "(define (f x y) (list x y))\n"
                       #:checker 'unused-argument #:expect '()))

    (test "accepts a parameter named with an underscore"
      (assert-warnings #:source "(define (f x _y) (list x))\n"
                       #:checker 'unused-argument #:expect '()))

    (test "reads the parameters of a defun"
      (assert-warnings #:source "(defun h (a b) a)\n" #:dialect 'elisp
                       #:checker 'unused-argument #:expect '((1 13))))

    (test "reads a parameter with a default"
      (assert-warnings #:source "(define* (g (a 1) #:key b) (list a))\n"
                       #:checker 'unused-argument #:expect '((1 25))))

    (test "reads a specialized Common Lisp parameter"
      (assert-warnings #:source "(defmethod area ((shape circle)) 1)\n"
                       #:dialect 'cl #:checker 'unused-argument #:expect '((1 19))))

    (test "accepts a lambda that uses its parameter"
      (assert-warnings #:source "(map (lambda (x) (* x 2)) xs)\n"
                       #:checker 'unused-argument #:expect '()))))
