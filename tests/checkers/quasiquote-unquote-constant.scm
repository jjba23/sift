;; sift --- checker: quasiquote-unquote-constant
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Unquoting a constant inside a quasiquote is a second thought that survived
;; into the source: the value would be the same written directly.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers quasiquote-unquote-constant)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: quasiquote-unquote-constant"

    (test "reports an unquoted number inside a quasiquote"
      (assert-warnings #:source "`(1 ,2)\n"
        #:checker 'quasiquote-unquote-constant
        #:expect '((1 5))))

    (test "reports the long-hand form too"
      (assert-warnings #:source "(quasiquote (unquote 2))\n"
        #:checker 'quasiquote-unquote-constant
        #:expect '((1 13))))

    (test "accepts an unquoted variable"
      (assert-warnings #:source "`(1 ,x)\n"
        #:checker 'quasiquote-unquote-constant #:expect '()))))
