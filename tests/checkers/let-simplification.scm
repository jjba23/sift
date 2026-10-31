;; sift --- checker: let-simplification
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; A `let` is a scope, and a scope around nothing is nothing.  The three
;; shapes below are the ones where the short form is not a matter of taste:
;; binding a name in order to return it, binding nothing at all, and a `let*`
;; with one binding, which binds once whatever the reader of `let*` expects.

(define-module (tests checkers let-simplification)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: let-simplification"

    (test "reports a binding that exists to be returned"
      (assert-warnings #:source "(let ((x (f))) x)\n"
        #:checker 'let-simplification
        #:expect '((1 1))))

    (test "the fix is the value"
      (assert-fix #:source "(let ((x (f))) x)\n" #:checker 'let-simplification
        #:expected "(f)\n"))

    (test "reports a let with no bindings"
      (assert-warnings #:source "(let () (f))\n" #:checker 'let-simplification
        #:expect '((1 1))))

    (test "the fix is the body"
      (assert-fix #:source "(let () (f))\n" #:checker 'let-simplification
        #:expected "(f)\n"))

    (test "reports a let* that binds once"
      (assert-warnings #:source "(let* ((x (f))) (g x))\n"
        #:checker 'let-simplification
        #:expect '((1 2))))

    (test "the fix says let"
      (assert-fix #:source "(let* ((x (f))) (g x))\n" #:checker 'let-simplification
        #:expected "(let ((x (f))) (g x))\n"))

    (test "the fixed files have nothing left to report"
      (for-each
         (lambda (source)
           (assert-fixes-to-nothing #:source source #:checker 'let-simplification
             #:name (format #f "~s is clean after its fix" source)))
         '("(let ((x (f))) x)\n" "(let () (f))\n" "(let* ((x (f))) (g x))\n")))

    (test "accepts a binding that is used in the body"
      (assert-warnings #:source "(let ((x (f))) (g x))\n"
        #:checker 'let-simplification
        #:expect '()))

    (test "accepts a let with two bindings"
      (assert-warnings #:source "(let ((x 1) (y 2)) x)\n"
        #:checker 'let-simplification
        #:expect '()))

    (test "accepts a let with a named let"
      (assert-warnings #:source "(let loop ((x 1)) (f x))\n"
        #:checker 'let-simplification
        #:expect '()))

    (test "accepts a let* with two bindings"
      (assert-warnings #:source "(let* ((x 1) (y 2)) (+ x y))\n"
        #:checker 'let-simplification
        #:expect '()))

    (test "an Emacs Lisp let is the same let"
      (assert-fix #:source "(let ((x (f))) x)\n" #:checker 'let-simplification
        #:dialect 'elisp
        #:expected "(f)\n"))))
