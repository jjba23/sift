;; sift --- checker: tabs
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; A tab in indentation renders at whatever width the reader's editor uses.
;; The fix replaces it with spaces at the configured indent width, and a tab
;; inside a string is left alone because it is data.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers tabs)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: tabs"

    (test "reports a tab used for indentation"
      (assert-warnings #:source "(define (f x)\n\tx)\n" #:checker 'tabs
        #:expect '((2 1))))

    (test "accepts indentation made of spaces"
      (assert-warnings #:source "(define (f x)\n  x)\n" #:checker 'tabs
        #:expect '()))

    (test "a tab inside a string is data, not indentation"
      (assert-warnings #:source "(display \"a\tb\")\n" #:checker 'tabs
        #:expect '()))

    (test "the fix replaces the tab with spaces"
      (assert-fix #:source "(a\n\tb)\n" #:checker 'tabs
        #:expected "(a\n  b)\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "(a\n\tb)\n" #:checker 'tabs))))
