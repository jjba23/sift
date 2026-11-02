;; sift --- checker: assoc-ref-outputs
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `(assoc-ref %outputs "out")` is the pre-gexp spelling of the output
;; directory: it works, and it is what the manual's own examples replaced with
;; `#$output` everywhere a gexp is available.

(define-module (tests checkers assoc-ref-outputs)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: assoc-ref-outputs"

    (test "reports the alist lookup inside a gexp"
      (assert-warnings #:source "#~(mkdir (assoc-ref %outputs \"out\"))\n"
                       #:checker 'assoc-ref-outputs
                       #:expect '((1 10))))

    (test "the fix writes `#$output`"
      (assert-fix #:source "#~(mkdir (assoc-ref %outputs \"out\"))\n"
                  #:checker 'assoc-ref-outputs
                  #:expected "#~(mkdir #$output)\n")
      (assert-fixes-to-nothing #:source "#~(mkdir (assoc-ref %outputs \"out\"))\n"
                               #:checker 'assoc-ref-outputs))

    (test "accepts `#$output`"
      (assert-warnings #:source "#~(mkdir #$output)\n"
                       #:checker 'assoc-ref-outputs
                       #:expect '()))

    (test "accepts a lookup of a build input"
      (assert-warnings #:source "#~(assoc-ref %build-inputs \"coreutils\")\n"
                       #:checker 'assoc-ref-outputs
                       #:expect '()))

    (test "leaves a lookup outside a build body alone"
      (assert-warnings #:source "(assoc-ref %outputs \"out\")\n"
                       #:checker 'assoc-ref-outputs
                       #:expect '()))

    ;; A build body written as a quasiquote -- what `gexp->derivation' took
    ;; before gexps -- names its output with `%output`, and the alist is the
    ;; same mistake it is in a gexp.
    (test "reports the alist in a quasiquoted build body"
      (assert-warning-count
       #:source "(gexp->derivation \"x\" `(mkdir (assoc-ref %outputs \"out\")))\n"
       #:checker 'assoc-ref-outputs
       #:count 1))

    (test "that body's fix writes `%output`"
      (assert-fix #:source "(gexp->derivation \"x\" `(mkdir (assoc-ref %outputs \"out\")))\n"
                  #:checker 'assoc-ref-outputs
                  #:expected "(gexp->derivation \"x\" `(mkdir %output))\n"))

    (test "a quasiquote outside a derivation is an ordinary list"
      (assert-warnings #:source "(list `(assoc-ref %outputs \"out\"))\n"
                       #:checker 'assoc-ref-outputs
                       #:expect '()))))
