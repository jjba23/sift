;; sift --- checker: ungexp-outside-gexp
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `#$`, `#$@`, `#+` and `#+@` have a meaning only where the reader was told a
;; gexp is being read.  Anywhere else they are tokens that name no form at all.

(define-module (tests checkers ungexp-outside-gexp)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: ungexp-outside-gexp"

    (test "reports `#$` with no gexp around it"
      (assert-warnings #:source "(list #$output)\n"
                       #:checker 'ungexp-outside-gexp
                       #:expect '((1 7))))

    (test "reports `#+@` with no gexp around it"
      (assert-warnings #:source "(list #+@ x)\n"
                       #:checker 'ungexp-outside-gexp
                       #:expect '((1 7))))

    (test "reports a gexp that lost its `#~`"
      (assert-warnings #:source "(define x #$output)\n"
                       #:checker 'ungexp-outside-gexp
                       #:expect '((1 11))))

    (test "accepts every form inside a gexp"
      (assert-warnings #:source "#~(list #$output #$@inputs #+sources #+@more)\n"
                       #:checker 'ungexp-outside-gexp
                       #:expect '()))

    (test "accepts the long-hand forms inside `gexp`"
      (assert-warnings #:source "(gexp (list (ungexp output)))\n"
                       #:checker 'ungexp-outside-gexp
                       #:expect '()))))
