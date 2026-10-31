;; sift --- checker: when-without-body
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `(when test)` with nothing after it is a form that does nothing at all.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers when-without-body)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: when-without-body"

    (test "reports a when with no body"
      (assert-warnings #:source "(when (ready?))\n" #:checker 'when-without-body
                       #:expect '((1 1))))

    (test "reports a when with no body in Common Lisp"
      (assert-warnings #:source "(when x)\n" #:dialect 'cl
                       #:checker 'when-without-body #:expect '((1 1))))

    (test "accepts a when with a body"
      (assert-warnings #:source "(when (ready?) (go))\n"
                       #:checker 'when-without-body #:expect '()))))
