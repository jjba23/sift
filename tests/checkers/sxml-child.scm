;; sift --- checker: sxml-child
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A child of an SXML element is an element, a string or a number.  A list
;; whose head is itself a list is a list of children written one level too
;; high: `((li "a") (li "b"))` where `(li "a") (li "b")` belongs.

(define-module (tests checkers sxml-child)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: sxml-child"

    (test "accepts elements, strings and numbers as children"
      (assert-warnings
       #:source "(div (@ (class \"x\")) (p \"a\") \"b\" 3)\n"
       #:checker 'sxml-child
       #:expect '()))

    (test "reports a list of elements where elements belong"
      (assert-warnings
       #:source "(ul (@ (id \"x\")) ((li \"a\") (li \"b\")))\n"
       #:checker 'sxml-child
       #:expect '((1 18))))

    (test "accepts the same list without the extra level"
      (assert-warnings
       #:source "(ul (@ (id \"x\")) (li \"a\") (li \"b\"))\n"
       #:checker 'sxml-child
       #:expect '()))

    (test "an element with no attributes is not judged"
      (assert-warnings #:source "(f ((g)))\n" #:checker 'sxml-child
                       #:expect '()))))
