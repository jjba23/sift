;; sift --- checker: declare-indent
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; `declare` follows the docstring and sits at the same indentation as the
;; body, two columns in from the `defun`.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers declare-indent)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: declare-indent"

    (test "reports a declare indented too far"
      (assert-warnings #:source "(defun f (x)\n    (declare (indent 1))\n  x)\n"
        #:dialect 'elisp #:checker 'declare-indent
        #:expect '((2 5))))

    (test "accepts a declare two columns in"
      (assert-warnings #:source "(defun f (x)\n  (declare (indent 1))\n  x)\n"
        #:dialect 'elisp #:checker 'declare-indent
        #:expect '()))

    (test "the fix re-indents the declare"
      (assert-fix #:source "(defun f (x)\n    (declare (indent 1))\n  x)\n"
        #:dialect 'elisp #:checker 'declare-indent
        #:expected "(defun f (x)\n  (declare (indent 1))\n  x)\n"))))
