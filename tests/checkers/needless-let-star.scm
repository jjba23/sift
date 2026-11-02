;; sift --- checker: needless-let*
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The other half of the `let' / `let*' choice: a `let*' no initializer
;; depends on is a `let', and the `let' is the form that says the order does
;; not matter.  One binding is `let-simplification`'s, so this asks about two.

(define-module (tests checkers needless-let-star)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: needless-let*"

    (test "reports a let* whose bindings are independent"
      (assert-warnings #:source "(let* ((a (f)) (b (g))) (+ a b))\n"
                       #:checker 'needless-let*
                       #:expect '((1 2))))

    (test "the fix says let"
      (assert-fix #:source "(let* ((a (f)) (b (g))) (+ a b))\n"
                  #:checker 'needless-let*
                  #:expected "(let ((a (f)) (b (g))) (+ a b))\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "(let* ((a (f)) (b (g))) (+ a b))\n"
                               #:checker 'needless-let*))

    (test "accepts a let* that depends on its own order"
      (assert-warnings #:source "(let* ((a (f)) (b (g a))) (+ a b))\n"
                       #:checker 'needless-let*
                       #:expect '()))

    (test "accepts a let* with one binding, which is another rule's"
      (assert-warnings #:source "(let* ((a (f))) a)\n"
                       #:checker 'needless-let*
                       #:expect '()))

    (test "an Emacs Lisp let* is the same let*"
      (assert-fix #:source "(let* ((a (f)) (b (g))) (+ a b))\n"
                  #:checker 'needless-let*
                  #:dialect 'elisp
                  #:expected "(let ((a (f)) (b (g))) (+ a b))\n"))))
