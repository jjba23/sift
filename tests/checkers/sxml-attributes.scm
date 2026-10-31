;; sift --- checker: sxml-attributes
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; An SXML element carries at most one attribute list, and it comes first:
;; `(div (@ (class "x")) (p "a"))`.  The evidence that a form is markup at all
;; is that attribute list, so ordinary code -- which has no `(@ ...)` in it --
;; is never judged by this rule.

(define-module (tests checkers sxml-attributes)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: sxml-attributes"

    (test "accepts an element whose attributes come first"
      (assert-warnings #:source "(div (@ (class \"x\")) (p \"a\"))\n"
        #:checker 'sxml-attributes
        #:expect '()))

    (test "accepts an element with no attributes at all"
      (assert-warnings #:source "(div (p \"a\"))\n" #:checker 'sxml-attributes
        #:expect '()))

    (test "reports attributes that come after a child"
      (assert-warnings #:source "(div (p \"a\") (@ (class \"x\")))\n"
        #:checker 'sxml-attributes
        #:expect '((1 14))))

    (test "reports a second attribute list"
      (assert-warnings #:source "(div (@ (class \"x\")) (@ (id \"y\")))\n"
        #:checker 'sxml-attributes
        #:expect '((1 22))))

    (test "reports an empty attribute list"
      (assert-warnings #:source "(div (@) (p \"a\"))\n" #:checker 'sxml-attributes
        #:expect '((1 6))))

    (test "accepts a document of nested elements"
      (assert-warnings
         #:source "(ul (@ (id \"x\"))\n    (li (@ (class \"a\")) \"one\")\n    (li \"two\"))\n"
         #:checker 'sxml-attributes
         #:expect '()))))
