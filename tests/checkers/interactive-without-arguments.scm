;; sift --- checker: interactive-without-arguments
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A command whose argument list has required parameters has to be told what
;; they are, and a bare `(interactive)' tells it nothing: Emacs calls the
;; function with no arguments and gets `wrong-number-of-arguments'.

(define-module (tests checkers interactive-without-arguments)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: interactive-without-arguments"

    (test "reports a command with a required argument and an empty spec"
      (assert-warnings
       #:source "(defun my-command (arg)\n  \"Do ARG.\"\n  (interactive)\n  (message \"%s\" arg))\n"
       #:checker 'interactive-without-arguments
       #:dialect 'elisp
       #:expect '((3 3))))

    (test "accepts a command with no parameters"
      (assert-warnings #:source "(defun my-command ()\n  \"Do it.\"\n  (interactive)\n  nil)\n"
                       #:checker 'interactive-without-arguments
                       #:dialect 'elisp
                       #:expect '()))

    (test "accepts a spec that supplies the argument"
      (assert-warnings
       #:source "(defun my-command (arg)\n  \"Do ARG.\"\n  (interactive \"p\")\n  (message \"%s\" arg))\n"
       #:checker 'interactive-without-arguments
       #:dialect 'elisp
       #:expect '()))

    (test "accepts optional parameters, which may be left out"
      (assert-warnings
       #:source "(defun my-command (&optional arg)\n  \"Do ARG.\"\n  (interactive)\n  (message \"%s\" arg))\n"
       #:checker 'interactive-without-arguments
       #:dialect 'elisp
       #:expect '()))))
