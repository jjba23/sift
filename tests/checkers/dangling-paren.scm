;; sift --- checker: dangling-paren
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Google's Common Lisp style guide and most community guides keep the
;; closing parenthesis on the line of the form it closes.  The fix pulls it up.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers dangling-paren)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: dangling-paren"

    (test "reports a closer that starts a line"
      (assert-warnings #:source "(defun f (x)\n  x\n  )\n" #:dialect 'cl
                       #:checker 'dangling-paren #:expect '((3 3))))

    (test "accepts a closer that trails the last element"
      (assert-warnings #:source "(defun f (x)\n  x)\n" #:dialect 'cl
                       #:checker 'dangling-paren #:expect '()))

    (test "the fix pulls the closer onto the previous line"
      (assert-fix #:source "(defun f (x)\n  x\n  )\n" #:dialect 'cl
                  #:checker 'dangling-paren #:expected "(defun f (x)\n  x)\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "(defun f (x)\n  x\n  )\n"
                               #:dialect 'cl #:checker 'dangling-paren))))
