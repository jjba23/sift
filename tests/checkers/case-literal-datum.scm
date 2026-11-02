;; sift --- checker: case-literal-datum
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A `case' clause is compared with `eqv?', which decides numbers, characters
;; and symbols.  A string datum is a clause that cannot match, however equal
;; the two strings are -- Guile's own compiler reports the same thing.

(define-module (tests checkers case-literal-datum)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: case-literal-datum"

    (test "reports a string datum"
      (assert-warnings #:source "(case x ((\"a\") 1))\n"
                       #:checker 'case-literal-datum
                       #:expect '((1 11))))

    (test "reports a string datum written without a list"
      (assert-warnings #:source "(case x (\"a\" 1) (else 2))\n"
                       #:checker 'case-literal-datum
                       #:expect '((1 10))))

    (test "accepts symbols and characters"
      (assert-warnings #:source "(case x ((a #\\b) 1))\n"
                       #:checker 'case-literal-datum
                       #:expect '()))

    (test "accepts a nullary case with a default"
      (assert-warnings #:source "(case x (else 2))\n"
                       #:checker 'case-literal-datum
                       #:expect '()))))
