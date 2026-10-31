;; sift --- checker: trailing-whitespace
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Lines that end with a space or a tab are invisible in an editor and show up
;; in every diff.  The fix removes the run, and the fixed file is clean.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers trailing-whitespace)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: trailing-whitespace"

    (test "reports a line that ends with a space"
      (assert-warnings #:source "x \n" #:checker 'trailing-whitespace
                       #:expect '((1 2))))

    (test "reports a line that ends with a tab"
      (assert-warnings #:source "x\t\n" #:checker 'trailing-whitespace
                       #:expect '((1 2))))

    (test "reports every line of a file"
      (assert-warnings #:source "a \nb \n" #:checker 'trailing-whitespace
                       #:expect '((1 2) (2 2))))

    (test "accepts a file whose lines are clean"
      (assert-warnings #:source "(define x 1)\n" #:checker 'trailing-whitespace
                       #:expect '()))

    (test "the fix strips the whitespace"
      (assert-fix #:source "x   \ny\n" #:checker 'trailing-whitespace
                  #:expected "x\ny\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "x   \n" #:checker 'trailing-whitespace))))
