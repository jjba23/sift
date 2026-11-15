;; sift -- checker: docstring-before-interactive
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Emacs reads a docstring from the first form in a body and nowhere else, so a
;; string written after `(interactive)' is a string and not documentation: the
;; command has no docstring and the text is dead.  Nothing about the file looks
;; wrong, which is why the rule exists -- the byte-compiler says nothing.
;;
;; The rule has no fix.  Moving the string above `(interactive)' is usually what
;; was meant, but the fix would have to decide where a docstring ends and the
;; body begins, and a formatter that guessed could move a string that is a value.

(define-module (tests checkers docstring-before-interactive)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: docstring-before-interactive"

    (test "reports a docstring written after interactive"
      (assert-warnings
       #:source "(defun f ()\n  (interactive)\n  \"Say hi.\"\n  (message \"hi\"))\n"
       #:dialect 'elisp
       #:checker 'docstring-before-interactive #:expect '((3 3))))

    (test "accepts the docstring at the head of the body"
      (assert-warnings
       #:source "(defun g ()\n  \"Say hi.\"\n  (interactive)\n  (message \"hi\"))\n"
       #:dialect 'elisp
       #:checker 'docstring-before-interactive #:expect '()))

    (test "accepts a string that is the body's last value"
      ;; `(message "hi")' returns a string too; the rule is about where the
      ;; first form is, and this one's first form is not a string
      (assert-warnings
       #:source "(defun h () (interactive) (message \"hi\"))\n"
       #:dialect 'elisp
       #:checker 'docstring-before-interactive #:expect '()))

    (test "reports a string in the body of a command, wherever it is written"
      (assert-warnings
       #:source "(defun i ()\n  (interactive)\n  (message \"hi\")\n  \"late\")\n"
       #:dialect 'elisp
       #:checker 'docstring-before-interactive #:expect '((4 3))))

    (test "says nothing outside Emacs Lisp"
      ;; a string after a form is an ordinary value in every other dialect
      (assert-warnings #:source "(define (f)\n  (go)\n  \"late\")\n"
                       #:checker 'docstring-before-interactive #:expect '()))))
