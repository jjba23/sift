;; sift --- checker: docstring-first-line
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The first line of a docstring is what `apropos` shows and what the manual
;; asks to be one or two complete sentences: capitalised, ending in a period,
;; and short enough to fit beside the command it describes.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers docstring-first-line)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: docstring-first-line"

    (test "reports a first line that starts lowercase"
      (assert-warnings #:source "(defun f (x)\n  \"returns x.\"\n  x)\n"
        #:dialect 'elisp #:checker 'docstring-first-line
        #:expect '((2 3))))

    (test "reports a first line with no full stop"
      (assert-warnings #:source "(defun f (x)\n  \"Return x\"\n  x)\n"
        #:dialect 'elisp #:checker 'docstring-first-line
        #:expect '((2 3))))

    (test "reports a first line that is too long for apropos"
      (assert-warnings
         #:source (string-append "(defun f (x)\n  \"" (make-string 80 #\X)
                    ".\"\n  x)\n")
         #:dialect 'elisp #:checker 'docstring-first-line
         #:expect '((2 3))))

    (test "accepts a capitalised sentence with a period"
      (assert-warnings #:source "(defun f (x)\n  \"Return X.\"\n  x)\n"
        #:dialect 'elisp #:checker 'docstring-first-line
        #:expect '()))))
