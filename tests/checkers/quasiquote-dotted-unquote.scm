;; sift --- checker: quasiquote-dotted-unquote
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Inside a quasiquote, `(key . ,value)' and `(key unquote value)' are the
;; same list: the tail `(unquote value)' is what the dot puts in the second
;; half, and the three elements are the same either way.  What the rule is
;; about is which of the two a reader can follow: the dot reads as two halves,
;; and it is the second half that is computed.

(define-module (tests checkers quasiquote-dotted-unquote)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: quasiquote-dotted-unquote"

    (test "reports a dotted pair whose tail is an unquote"
      (assert-warnings #:source "`((key . ,value))\n"
                       #:checker 'quasiquote-dotted-unquote
                       #:expect '((1 3))))

    (test "accepts the pair written with unquote"
      (assert-warnings #:source "`((key unquote value))\n"
                       #:checker 'quasiquote-dotted-unquote
                       #:expect '()))

    (test "accepts a dotted pair that is data"
      (assert-warnings #:source "`((key . value))\n"
                       #:checker 'quasiquote-dotted-unquote
                       #:expect '()))

    (test "accepts a dotted pair outside a quasiquote"
      (assert-warnings #:source "(list (key . ,value))\n"
                       #:checker 'quasiquote-dotted-unquote
                       #:expect '()))

    (test "the fix writes the unquote out"
      (assert-fix #:source "`((key . ,value))\n"
                  #:checker 'quasiquote-dotted-unquote
                  #:expected "`((key unquote value))\n"))

    (test "a comment between the dot and the comma is kept"
      (assert-fix #:source "`((key . ;; why\n   ,value))\n"
                  #:checker 'quasiquote-dotted-unquote
                  #:expected "`((key . ;; why\n   ,value))\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "`((key . ,value))\n"
                               #:checker 'quasiquote-dotted-unquote))))
