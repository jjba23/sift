;; sift --- checker: reader-prefix-break
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A quote, a backquote or a `#' belongs to the datum after it; a newline
;; between them reads as a break at the wrong level for anyone scanning the
;; file.  The fix joins them, and the formatter already does.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers reader-prefix-break)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: reader-prefix-break"

    (test "reports a quote separated from its datum"
      (assert-warnings #:source "'\n(a)\n" #:checker 'reader-prefix-break
        #:expect '((1 1))))

    (test "accepts a quote attached to its datum"
      (assert-warnings #:source "'(a)\n" #:checker 'reader-prefix-break
        #:expect '()))

    (test "the fix joins the prefix to the datum"
      (assert-fix #:source "'\n(a)\n" #:checker 'reader-prefix-break
        #:expected "'(a)\n"))

    (test "a function quote is covered too"
      (assert-warnings #:source "#'\nfoo\n" #:dialect 'cl
        #:checker 'reader-prefix-break #:expect '((1 1))))))
