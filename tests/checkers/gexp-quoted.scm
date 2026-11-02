;; sift --- checker: gexp-quoted
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `'#~(...)' quotes the *form*, so what the build system receives is a list
;; beginning with `gexp' rather than a gexp object.  The manual's own warning
;; is that such a gexp "is not evaluated"; what makes the rule worth a lint is
;; that the mistake is invisible in a diff.

(define-module (tests checkers gexp-quoted)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: gexp-quoted"

    (test "reports a gexp written under a quote"
      (assert-warnings #:source "(define x '#~(mkdir #$output))\n"
                       #:checker 'gexp-quoted
                       #:expect '((1 12))))

    (test "reports a gexp under a quasiquote"
      (assert-warnings #:source "(list `#~x)\n"
                       #:checker 'gexp-quoted
                       #:expect '((1 8))))

    (test "accepts a gexp where it is used"
      (assert-warnings #:source "#~(mkdir #$output)\n"
                       #:checker 'gexp-quoted
                       #:expect '()))

    (test "accepts the long-hand `gexp`"
      (assert-warnings #:source "(gexp (mkdir (ungexp output)))\n"
                       #:checker 'gexp-quoted
                       #:expect '()))))
