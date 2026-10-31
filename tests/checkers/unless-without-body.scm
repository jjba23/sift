;; sift --- checker: unless-without-body
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; `(unless test)` with nothing after it is the mirror image of an empty
;; `when`, and just as useless.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers unless-without-body)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: unless-without-body"

    (test "reports an unless with no body"
      (assert-warnings #:source "(unless (ready?))\n"
        #:checker 'unless-without-body #:expect '((1 1))))

    (test "reports one in Common Lisp too"
      (assert-warnings #:source "(unless x)\n" #:dialect 'cl
        #:checker 'unless-without-body #:expect '((1 1))))

    (test "accepts an unless with a body"
      (assert-warnings #:source "(unless (ready?) (wait))\n"
        #:checker 'unless-without-body #:expect '()))))
