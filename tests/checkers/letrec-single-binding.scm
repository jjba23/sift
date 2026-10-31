;; sift --- checker: letrec-single-binding
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; A `letrec` with one binding that never refers to itself is a `let` written
;; the hard way, and it tells the reader something untrue about the binding.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers letrec-single-binding)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: letrec-single-binding"

    (test "reports a letrec with one non-recursive binding"
      (assert-warnings #:source "(letrec ((x 1)) x)\n"
        #:checker 'letrec-single-binding #:expect '((1 1))))

    (test "accepts a binding that refers to itself"
      (assert-warnings #:source "(letrec ((f (lambda () (f)))) (f))\n"
        #:checker 'letrec-single-binding #:expect '()))

    (test "accepts two bindings"
      (assert-warnings #:source "(letrec ((a 1) (b 2)) (+ a b))\n"
        #:checker 'letrec-single-binding #:expect '()))))
