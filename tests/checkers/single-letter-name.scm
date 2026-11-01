;; sift --- checker: single-letter-name
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; One letter is a placeholder for a name, not a name.  The rule looks at the
;; places a reader reads as a variable -- the parameters of a procedure and
;; the names of a binding list -- and never at a use, because a use is
;; whatever the binding said it was.
;;
;; Index variables are the exception, and which letters a project reads that
;; way is a setting; `i' and `j' are the default.

(define-module (tests checkers single-letter-name)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: single-letter-name"

    (test "reports each single-letter parameter of a definition"
      (assert-warnings #:source "(define (area w h) (* w h))\n"
                       #:checker 'single-letter-name
                       #:expect '((1 15) (1 17))))

    (test "accepts names made of words"
      (assert-warnings #:source "(define (area width height) (* width height))\n"
                       #:checker 'single-letter-name #:expect '()))

    (test "accepts an index variable, which is what the setting is for"
      (assert-warnings #:source "(let ((i 0)) i)\n"
                       #:checker 'single-letter-name #:expect '()))

    (test "reports the binding of a `let'"
      (assert-warnings #:source "(let ((x 1)) x)\n"
                       #:checker 'single-letter-name #:expect '((1 8))))

    (test "reports a parameter of a `lambda'"
      (assert-warnings #:source "(lambda (x) x)\n"
                       #:checker 'single-letter-name #:expect '((1 10))))

    (test "reports a parameter of an Emacs Lisp defun"
      (assert-warnings #:source "(defun f (n) n)\n" #:dialect 'elisp
                       #:checker 'single-letter-name #:expect '((1 11))))

    (test "reports a parameter of a Common Lisp defun"
      (assert-warnings #:source "(defun f (a) a)\n" #:dialect 'cl
                       #:checker 'single-letter-name #:expect '((1 11))))

    (test "accepts an ignored parameter"
      (assert-warnings #:source "(lambda (_ value) value)\n" #:dialect 'elisp
                       #:checker 'single-letter-name #:expect '()))

    (test "the accepted names are a setting"
      (assert-warnings #:source "(lambda (x) x)\n"
                       #:checker 'single-letter-name
                       #:options '((single-letter-names . (x)))
                       #:expect '()))

    (test "a setting replaces the default list rather than extending it"
      (assert-warnings #:source "(lambda (i) i)\n"
                       #:checker 'single-letter-name
                       #:options '((single-letter-names . (x)))
                       #:expect '((1 10))))

    (test "says nothing about a definition that names a program"
      (assert-warnings #:source "(define x 1)\n"
                       #:checker 'single-letter-name #:expect '()))))
