;; sift --- checker: number-literal-argument
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The mirror of `string-literal-argument': `+', `=' and `modulo' work on
;; numbers, and a string literal among their arguments is a call that warns at
;; run time or, in Scheme, raises.

(define-module (tests checkers number-literal-argument)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: number-literal-argument"

    (test "reports a string added to a number"
      (assert-warning-count #:source "(+ 1 \"2\")\n"
                            #:checker 'number-literal-argument
                            #:count 1))

    (test "reports a string compared with a number"
      (assert-warning-count #:source "(= n \"3\")\n"
                            #:checker 'number-literal-argument
                            #:count 1))

    (test "accepts numbers"
      (assert-warnings #:source "(+ 1 2)\n"
                       #:checker 'number-literal-argument
                       #:expect '()))

    (test "leaves a variable alone"
      (assert-warnings #:source "(- n 1)\n"
                       #:checker 'number-literal-argument
                       #:expect '()))

    (test "does not read `string-append` as arithmetic"
      (assert-warnings #:source "(string-append \"1\" \"2\")\n"
                       #:checker 'number-literal-argument
                       #:expect '()))))
