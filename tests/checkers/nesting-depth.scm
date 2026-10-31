;; sift -- checker: nesting-depth
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A Lisp nests: a `let' inside a `cond' clause inside a `define' is ordinary
;; code.  What this rule counts is the number of enclosing *conditionals* --
;; a decision inside a decision inside a decision -- because that is the shape
;; a reader has to hold in their head, and parenthesis depth is not.

(define-module (tests checkers nesting-depth)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define deep-if
  "(define (f x)\n  (if (a)\n    (if (b)\n      (if (c)\n        (if (d)\n          (if (e) 1 2))))))\n")

(define (spec)
  (suite "checker: nesting-depth"

    (test "reports a chain of conditionals deeper than the limit"
      (assert-warnings #:source deep-if #:checker 'nesting-depth
        #:expect '((1 1))))

    (test "the limit is configuration"
      (assert-warnings #:source deep-if #:checker 'nesting-depth
        #:options '((max-definition-depth . 9)) #:expect '()))

    (test "accepts a `let' inside a `cond' inside a `define'"
      ;; parentheses are not the measure: this is three levels of
      ;; parentheses and one conditional
      (assert-warnings
         #:source "(define (f x)\n  (let ((y (car x)))\n    (cond [(null? y) 1]\n          [else 2])))\n"
         #:checker 'nesting-depth #:expect '()))

    (test "counts a `when' inside an `if'"
      (assert-warnings
         #:source "(define (f x)\n  (when (a)\n    (if (b)\n      (when (c)\n        (if (d)\n          (if (e) 1 2))))))\n"
         #:checker 'nesting-depth #:expect '((1 1))))))
