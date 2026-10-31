;; sift --- checker: defun-no-docstring
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Common Lisp does not require a docstring, but a definition without one is
;; a definition nobody can find out about from the running image.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers defun-no-docstring)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: defun-no-docstring"

    (test "reports a defun with no docstring"
      (assert-warnings #:source "(defun f (x) (+ x 1))\n" #:dialect 'cl
        #:checker 'defun-no-docstring #:expect '((1 1))))

    (test "accepts a documented defun"
      (assert-warnings #:source "(defun f (x)\n  \"Return X.\"\n  x)\n"
        #:dialect 'cl #:checker 'defun-no-docstring
        #:expect '()))))
