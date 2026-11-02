;; sift --- checker: eq-non-symbol
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `eq' is identity.  Two separately read numbers need not be the same object
;; and two equal strings almost never are, so a comparison against a literal is
;; decidable from the text: the predicate the literal's type asks for is the
;; one to write, and the fix is that predicate.

(define-module (tests checkers eq-non-symbol)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: eq-non-symbol"

    (test "reports `eq?` against a number"
      (assert-warnings #:source "(eq? x 1)\n"
                       #:checker 'eq-non-symbol
                       #:expect '((1 2))))

    (test "the fix writes `eqv?`"
      (assert-fix #:source "(eq? x 1)\n"
                  #:checker 'eq-non-symbol
                  #:expected "(eqv? x 1)\n")
      (assert-fixes-to-nothing #:source "(eq? x 1)\n"
                               #:checker 'eq-non-symbol))

    (test "a string asks for `equal?`"
      (assert-fix #:source "(eq? x \"a\")\n"
                  #:checker 'eq-non-symbol
                  #:expected "(equal? x \"a\")\n"))

    (test "Common Lisp asks for `eql`"
      (assert-fix #:source "(eq x 1.0)\n"
                  #:checker 'eq-non-symbol
                  #:dialect 'cl
                  #:expected "(eql x 1.0)\n"))

    (test "`memq` of a string asks for `member`"
      (assert-fix #:source "(memq \"a\" xs)\n"
                  #:checker 'eq-non-symbol
                  #:expected "(member \"a\" xs)\n"))

    (test "accepts a symbol, which identity does decide"
      (assert-warnings #:source "(eq? x 'ready)\n"
                       #:checker 'eq-non-symbol
                       #:expect '()))

    (test "accepts two variables"
      (assert-warnings #:source "(eq? x y)\n"
                       #:checker 'eq-non-symbol
                       #:expect '()))))
