;; sift --- checker: redundant-progn
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `progn` as the only body of a form that already has an implicit one says
;; nothing twice.  The fix splices its body into the enclosing form.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers redundant-progn)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: redundant-progn"

    (test "reports a progn that is the only body form"
      (assert-warnings #:source "(define (f) (progn (a) (b)))\n"
        #:checker 'redundant-progn #:expect '((1 13))))

    (test "reports one in a defun"
      (assert-warnings #:source "(defun f () (progn (a)))\n" #:dialect 'elisp
        #:checker 'redundant-progn #:expect '((1 13))))

    (test "accepts a progn with company"
      (assert-warnings #:source "(define (f) (a) (progn (b)))\n"
        #:checker 'redundant-progn #:expect '()))

    (test "the fix splices the body"
      (assert-fix #:source "(define (f) (progn (a) (b)))\n"
        #:checker 'redundant-progn
        #:expected "(define (f) (a) (b))\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "(define (f) (progn (a) (b)))\n"
        #:checker 'redundant-progn))))
