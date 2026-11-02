;; sift --- checker: string-literal-argument
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `string-append' appends strings and `string-length' measures one: the
;; convention is in the name.  A rule cannot use that to type a variable -- and
;; does not try -- but a number where the convention says a string is a fact
;; about the text in front of the reader.

(define-module (tests checkers string-literal-argument)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: string-literal-argument"

    (test "reports a number passed to `string-append`"
      (assert-warning-count #:source "(string-append \"a\" 1)\n"
                            #:checker 'string-literal-argument
                            #:count 1))

    (test "reports a boolean passed to `string-length`"
      (assert-warning-count #:source "(string-length #t)\n"
                            #:checker 'string-literal-argument
                            #:count 1))

    (test "accepts strings"
      (assert-warnings #:source "(string-append \"a\" \"b\")\n"
                       #:checker 'string-literal-argument
                       #:expect '()))

    (test "leaves a variable alone, whatever it holds"
      (assert-warnings #:source "(string-append \"a\" b)\n"
                       #:checker 'string-literal-argument
                       #:expect '()))

    (test "does not read an index as a string"
      (assert-warnings #:source "(string-ref \"abc\" 0)\n"
                       #:checker 'string-literal-argument
                       #:expect '()))

    (test "reports the character position a comparison takes"
      (assert-warnings #:source "(string=? \"a\" 1)\n"
                       #:checker 'string-literal-argument
                       #:expect '((1 15))))))
