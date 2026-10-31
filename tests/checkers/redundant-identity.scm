;; sift --- checker: redundant-identity
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; `(identity x)` is `x`, and `(funcall #'foo x)` is `(foo x)`: both say
;; something the reader has to undo.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers redundant-identity)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: redundant-identity"

    (test "reports an identity call"
      (assert-warnings #:source "(mapcar (lambda (x) (identity x)) xs)\n"
        #:dialect 'cl #:checker 'redundant-identity
        #:expect '((1 21))))

    (test "reports a funcall of a function name"
      (assert-warnings #:source "(funcall #'foo 1)\n" #:dialect 'cl
        #:checker 'redundant-identity #:expect '((1 1))))

    (test "accepts a plain call"
      (assert-warnings #:source "(foo 1)\n" #:dialect 'cl
        #:checker 'redundant-identity #:expect '()))))
