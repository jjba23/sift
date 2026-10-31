;; sift --- checker: interactive-without-docstring
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; A command is what a user finds with `M-x`, and the docstring is the only
;; thing that says what it does.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers interactive-without-docstring)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: interactive-without-docstring"

    (test "reports an interactive command with no docstring"
      (assert-warnings #:source "(defun c ()\n  (interactive)\n  (message \"hi\"))\n"
        #:dialect 'elisp #:checker 'interactive-without-docstring
        #:expect '((2 3))))

    (test "accepts a documented command"
      (assert-warnings
         #:source "(defun c ()\n  \"Say hi.\"\n  (interactive)\n  (message \"hi\"))\n"
         #:dialect 'elisp #:checker 'interactive-without-docstring #:expect '()))))
