;; sift --- checker: docstring-arguments
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; checkdoc's oldest rule: a docstring that does not mention an argument
;; leaves the reader of `C-h f` to guess what the argument is for.  Emacs
;; writes arguments in upcase, but this rule accepts any spelling -- the
;; point is that the name is there at all.

(define-module (tests checkers docstring-arguments)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: docstring-arguments"

    (test "reports each argument the docstring does not mention"
      (assert-warnings #:source "(defun add (a b)\n  \"Add two numbers.\"\n  (+ a b))\n"
        #:dialect 'elisp #:checker 'docstring-arguments
        #:expect '((2 3) (2 3))))

    (test "accepts a docstring that names its arguments"
      (assert-warnings #:source "(defun add (a b)\n  \"Add A and B.\"\n  (+ a b))\n"
        #:dialect 'elisp #:checker 'docstring-arguments #:expect '()))

    (test "accepts any spelling of the name"
      (assert-warnings #:source "(defun add (a b)\n  \"Add a and b.\"\n  (+ a b))\n"
        #:dialect 'elisp #:checker 'docstring-arguments #:expect '()))

    (test "looks through optional and rest arguments"
      (assert-warnings
         #:source "(defun go (a &optional b &rest rest)\n  \"Go with A and B.\"\n  nil)\n"
         #:dialect 'elisp #:checker 'docstring-arguments #:expect '((2 3))))

    (test "says nothing about a defun with no docstring"
      (assert-warnings #:source "(defun add (a b) (+ a b))\n" #:dialect 'elisp
        #:checker 'docstring-arguments #:expect '()))))
