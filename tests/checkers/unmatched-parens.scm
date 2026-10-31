;; sift --- checker: unmatched-parens
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; sift never guesses where a missing delimiter belongs: the finding is
;; reported where the form was opened, and there is no fix.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers unmatched-parens)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: unmatched-parens"

    (test "reports an unmatched opening parenthesis at the opener"
      (assert-warnings #:source "(a (b c" #:checker 'unmatched-parens
                       #:expect '((1 1) (1 4))))

    (test "reports a stray closing parenthesis"
      (assert-warnings #:source "a) b" #:checker 'unmatched-parens
                       #:expect '((1 2))))

    (test "accepts a balanced file"
      (assert-warnings #:source "(a (b c))\n" #:checker 'unmatched-parens
                       #:expect '()))))
