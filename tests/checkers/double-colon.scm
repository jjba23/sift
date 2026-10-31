;; sift --- checker: double-colon
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; A double colon reaches past the package boundary into a symbol that was
;; never exported; it is for debugging, not for production code.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers double-colon)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: double-colon"

    (test "reports a double-colon reference"
      (assert-warnings #:source "(other::internal 1)\n" #:dialect 'cl
        #:checker 'double-colon #:expect '((1 2))))

    (test "accepts a single-colon reference"
      (assert-warnings #:source "(other:public 1)\n" #:dialect 'cl
        #:checker 'double-colon #:expect '()))

    (test "accepts an ordinary symbol"
      (assert-warnings #:source "(list 1 2)\n" #:dialect 'cl
        #:checker 'double-colon #:expect '()))))
