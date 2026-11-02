;; sift --- checker: eq-nil
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `(eq? x '())' asks whether two objects are the same one, and the empty list
;; is not promised to be a single object.  What the author meant is whether the
;; value is empty, which `null?' answers in Scheme and `null' answers in the
;; two dialects where `nil' is also the whole of falsehood.

(define-module (tests checkers eq-nil)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: eq-nil"

    (test "reports an eq? against the empty list"
      (assert-warnings #:source "(eq? x '())\n"
                       #:checker 'eq-nil
                       #:expect '((1 1))))

    (test "the fix asks null?"
      (assert-fix #:source "(eq? x '())\n"
                  #:checker 'eq-nil
                  #:expected "(null? x)\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "(eq? x '())\n" #:checker 'eq-nil))

    (test "an Emacs Lisp eq against nil asks null"
      (assert-fix #:source "(eq x nil)\n"
                  #:checker 'eq-nil
                  #:dialect 'elisp
                  #:expected "(null x)\n"))

    (test "a Common Lisp eq against nil asks null"
      (assert-fix #:source "(eq nil y)\n"
                  #:checker 'eq-nil
                  #:dialect 'cl
                  #:expected "(null y)\n"))

    (test "accepts a comparison with another symbol"
      (assert-warnings #:source "(eq? x y)\n"
                       #:checker 'eq-nil
                       #:expect '()))

    (test "accepts a comparison with #f"
      (assert-warnings #:source "(eq? x #f)\n"
                       #:checker 'eq-nil
                       #:expect '()))))
