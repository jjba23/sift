;; sift --- checker: trailing-blank-lines
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Blank lines at the end of a file are invisible in an editor and show up as
;; noise in every diff.  One blank line at the end is a convention worth
;; keeping; more than two is a file that grew by appending.
;;
;; The finding is counted rather than positioned: where a file ends is the
;; least interesting thing about it, and the count is what the message says.

(define-module (tests checkers trailing-blank-lines)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: trailing-blank-lines"

    (test "reports four blank lines at the end of a file"
      (assert-warning-count #:source "(define x 1)\n\n\n\n\n"
                            #:checker 'trailing-blank-lines #:count 1))

    (test "accepts a file with a single trailing newline"
      (assert-warning-count #:source "(define x 1)\n"
                            #:checker 'trailing-blank-lines #:count 0))

    (test "accepts a file with one blank line at the end"
      (assert-warning-count #:source "(define x 1)\n\n"
                            #:checker 'trailing-blank-lines #:count 0))

    (test "the fix trims the end of the file to one blank line"
      (assert-fix #:source "(define x 1)\n\n\n\n\n"
                  #:checker 'trailing-blank-lines
                  #:expected "(define x 1)\n\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "(define x 1)\n\n\n\n\n"
                               #:checker 'trailing-blank-lines))))
