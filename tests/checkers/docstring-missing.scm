;; sift --- checker: docstring-missing
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The Emacs Lisp Reference Manual asks every command, function and variable
;; intended for users to carry a docstring, because that is what `C-h f`
;; shows.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers docstring-missing)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: docstring-missing"

    (test "reports a defun with no docstring"
      (assert-warnings #:source "(defun f (x) (+ x 1))\n" #:dialect 'elisp
        #:checker 'docstring-missing #:expect '((1 1))))

    (test "reports a defvar with no docstring"
      (assert-warnings #:source "(defvar x 1)\n" #:dialect 'elisp
        #:checker 'docstring-missing #:expect '((1 1))))

    (test "reports a defcustom with no docstring"
      (assert-warnings #:source "(defcustom x 1)\n" #:dialect 'elisp
        #:checker 'docstring-missing #:expect '((1 1))))

    (test "accepts a documented defun"
      (assert-warnings #:source "(defun f (x)\n  \"Return X.\"\n  x)\n"
        #:dialect 'elisp #:checker 'docstring-missing
        #:expect '()))

    (test "accepts a bare defvar declaration"
      (assert-warnings #:source "(defvar x)\n" #:dialect 'elisp
        #:checker 'docstring-missing #:expect '()))))
