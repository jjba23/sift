;; sift --- checker: empty-body
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; A body that is missing is usually a form that was written for its effect
;; and then edited.  In Emacs Lisp this checker covers `when` and `unless` as
;; well, because the dedicated checkers are Scheme and Common Lisp ones.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers empty-body)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: empty-body"

    (test "reports a cond clause with no body"
      (assert-warnings #:source "(cond (a))\n" #:checker 'empty-body
        #:expect '((1 7))))

    (test "reports a let with no body"
      (assert-warnings #:source "(let ((x 1)))\n" #:checker 'empty-body
        #:expect '((1 1))))

    (test "reports a lambda with no body"
      (assert-warnings #:source "(lambda (x))\n" #:checker 'empty-body
        #:expect '((1 1))))

    (test "reports a cond with no clauses"
      (assert-warnings #:source "(cond)\n" #:checker 'empty-body
        #:expect '((1 1))))

    (test "covers when and unless in Emacs Lisp"
      (assert-warnings #:source "(when x)\n" #:dialect 'elisp
        #:checker 'empty-body #:expect '((1 1))))

    (test "a case datum is data, not a form"
      ;; `(cond)` here is a list of values to match, not a `cond` form
      (assert-warnings #:source "(case x\n  [(cond) 1]\n  [else 2])\n"
        #:checker 'empty-body #:expect '()))

    (test "accepts a form with a body"
      (assert-warnings #:source "(cond (a b))\n" #:checker 'empty-body
        #:expect '()))))
