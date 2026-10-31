;; sift --- checker: docstring-indent
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The manual warns against indenting continuation lines of a docstring to
;; line up with the opening quote: the indentation ends up in the manual and in
;; `C-h f`.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers docstring-indent)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: docstring-indent"

    (test "reports an indented continuation line"
      (assert-warnings #:source "(defun f (x)\n  \"Doc.\n    More.\"\n  x)\n"
                       #:dialect 'elisp #:checker 'docstring-indent
                       #:expect '((2 3))))

    (test "accepts continuation lines that start at column zero"
      (assert-warnings #:source "(defun f (x)\n  \"Doc.\nMore.\"\n  x)\n"
                       #:dialect 'elisp #:checker 'docstring-indent
                       #:expect '()))))
