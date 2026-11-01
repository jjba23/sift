;; sift --- checker: empty-form
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `()` in a value position is nearly always a missing quote: Scheme's empty
;; list is not a constant, so `(list ())` says something the author did not
;; mean.  Every form that legitimately holds an empty list -- a parameter list,
;; a binding list, the literals of a macro transformer, quoted data -- is left
;; alone, because there the `()` is the right thing to have written.

(define-module (tests checkers empty-form)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: empty-form"

    (test "reports the empty list in an argument position"
      (assert-warnings #:source "(list ())\n" #:checker 'empty-form
                       #:expect '((1 7))))

    (test "accepts a quoted empty list"
      (assert-warnings #:source "(list '())\n" #:checker 'empty-form
                       #:expect '()))

    (test "accepts an empty parameter list"
      (assert-warnings #:source "(lambda () 1)\n" #:checker 'empty-form
                       #:expect '()))

    (test "accepts an empty binding list"
      (assert-warnings #:source "(let () 1)\n" #:checker 'empty-form
                       #:expect '()))

    (test "accepts the literals list of a syntax-rules transformer"
      (assert-warnings #:source "(syntax-rules () ((_) 'done))\n"
                       #:checker 'empty-form
                       #:expect '()))

    (test "accepts the literals list of a syntax-case transformer"
      (assert-warnings #:source "(syntax-case stx () [(_ x) x])\n"
                       #:checker 'empty-form
                       #:expect '()))))
