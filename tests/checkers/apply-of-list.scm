;; sift --- checker: apply-of-list
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `(apply f a (list b c))' builds a list in order to spread it again, and
;; `(apply f '())' spreads nothing at all.  Both are a call the reader can
;; write directly, and the fix is that call.

(define-module (tests checkers apply-of-list)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: apply-of-list"

    (test "reports an apply of a list"
      (assert-warnings #:source "(apply f 1 (list 2 3))\n"
                       #:checker 'apply-of-list
                       #:expect '((1 1))))

    (test "the fix writes the call"
      (assert-fix #:source "(apply f 1 (list 2 3))\n"
                  #:checker 'apply-of-list
                  #:expected "(f 1 2 3)\n"))

    (test "reports an apply of the empty list"
      (assert-fix #:source "(apply f '())\n"
                  #:checker 'apply-of-list
                  #:expected "(f)\n"))

    (test "the fixed files have nothing left to report"
      (for-each
       (lambda (source)
         (assert-fixes-to-nothing #:source source #:checker 'apply-of-list
                                  #:name (format #f "~s is clean after its fix" source)))
       '("(apply f 1 (list 2 3))\n" "(apply f '())\n")))

    (test "accepts an apply of a variable"
      (assert-warnings #:source "(apply f xs)\n"
                       #:checker 'apply-of-list
                       #:expect '()))

    (test "accepts a list argument that is not the last one"
      (assert-warnings #:source "(apply f (list 1 2) 3)\n"
                       #:checker 'apply-of-list
                       #:expect '()))

    (test "an Emacs Lisp apply is the same apply"
      (assert-fix #:source "(apply 'f (list 1 2))\n"
                  #:checker 'apply-of-list
                  #:dialect 'elisp
                  #:expected "('f 1 2)\n"))))
