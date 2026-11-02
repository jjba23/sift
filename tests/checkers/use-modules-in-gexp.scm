;; sift --- checker: use-modules-in-gexp
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A `use-modules' inside a gexp is read by the builder, where only the modules
;; the gexp was marked with exist.  `with-imported-modules' is that marking,
;; and its scope is lexical: the gexp has to be written inside it.

(define-module (tests checkers use-modules-in-gexp)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: use-modules-in-gexp"

    (test "reports a build module imported inside a bare gexp"
      (assert-warnings #:source "#~(use-modules (guix build utils))\n"
                       #:checker 'use-modules-in-gexp
                       #:expect '((1 3))))

    (test "accepts a gexp marked with `with-imported-modules`"
      (assert-warnings #:source "(with-imported-modules '((guix build utils))\n  #~(use-modules (guix build utils)))\n"
                       #:checker 'use-modules-in-gexp
                       #:expect '()))

    (test "accepts the extension modules too"
      (assert-warnings #:source "(with-extensions (list guile-json)\n  #~(use-modules (json)))\n"
                       #:checker 'use-modules-in-gexp
                       #:expect '()))

    (test "leaves a library of the host stratum alone"
      (assert-warnings #:source "#~(use-modules (srfi srfi-1))\n"
                       #:checker 'use-modules-in-gexp
                       #:expect '()))

    (test "reports it in a quasiquoted build body"
      (assert-warning-count
       #:source "(gexp->derivation \"x\" `(use-modules (guix build utils)))\n"
       #:checker 'use-modules-in-gexp
       #:count 1))

    (test "accepts a body the builder was given the modules for"
      (assert-warnings
       #:source "(gexp->derivation \"x\" `(use-modules (guix build utils)) #:modules '((guix build utils)))\n"
       #:checker 'use-modules-in-gexp
       #:expect '()))))
