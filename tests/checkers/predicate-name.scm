;; sift -- checker: predicate-name
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; A Lisp has no return type to read, so the name carries it: `positive?'
;; says the value is a boolean and `positive' says it is a number.  The rule
;; reads the *last* form of the body, because that is the value returned, and
;; asks whether it is a comparison or another predicate.

(define-module (tests checkers predicate-name)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: predicate-name"

    (test "reports a procedure that returns a comparison"
      (assert-warnings #:source "(define (positive x) (> x 0))\n"
        #:checker 'predicate-name #:expect '((1 10))))

    (test "reports a procedure whose last form is another predicate"
      (assert-warnings #:source "(define (member-in x xs) (memq x xs))\n"
        #:checker 'predicate-name #:expect '((1 10))))

    (test "accepts a name that ends in a question mark"
      (assert-warnings #:source "(define (positive? x) (> x 0))\n"
        #:checker 'predicate-name #:expect '()))

    (test "accepts a procedure that returns something else"
      (assert-warnings #:source "(define (double x) (* 2 x))\n"
        #:checker 'predicate-name #:expect '()))

    (test "Emacs Lisp accepts the -p suffix"
      (assert-warnings #:source "(defun string-empty-p (s) (= 0 (string-length s)))\n"
        #:dialect 'elisp #:checker 'predicate-name #:expect '()))

    (test "Emacs Lisp reports a name with neither suffix"
      (assert-warnings #:source "(defun string-empty (s) (= 0 (string-length s)))\n"
        #:dialect 'elisp #:checker 'predicate-name #:expect '((1 8))))

    (test "a docstring is not the returned value"
      (assert-warnings #:source "(define (positive x)\n  \"Return true if X is positive.\"\n  (> x 0))\n"
        #:checker 'predicate-name #:expect '((1 10))))))
