;; sift --- checker: blank-line-in-body
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A blank line inside a body is a paragraph break inside a paragraph.  The
;; one place it is allowed is between two internal definitions, where there
;; really are two things to separate -- so the rule has to be silent there,
;; and it has to be silent outside a body too: a blank line between two
;; top-level forms, or between the bindings of a `let', is the author's
;; layout and not this rule's business.

(define-module (tests checkers blank-line-in-body)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: blank-line-in-body"

    (test "reports a blank line between two body forms of a definition"
      (assert-warnings
       #:source "(define (f x)\n  (first x)\n\n  (second x))\n"
       #:checker 'blank-line-in-body #:expect '((4 3))))

    (test "accepts a body whose forms are adjacent"
      (assert-warnings
       #:source "(define (f x)\n  (first x)\n  (second x))\n"
       #:checker 'blank-line-in-body #:expect '()))

    (test "accepts a blank line between two internal definitions"
      (assert-warnings
       #:source "(define (f x)\n  (define a 1)\n\n  (define b 2)\n  (+ a b))\n"
       #:checker 'blank-line-in-body #:expect '()))

    (test "reports a blank line in a `let' body"
      (assert-warnings
       #:source "(let ((a 1))\n  (first a)\n\n  (second a))\n"
       #:checker 'blank-line-in-body #:expect '((4 3))))

    (test "reports a blank line in a `lambda' body"
      (assert-warnings
       #:source "(lambda (x)\n  (first x)\n\n  (second x))\n"
       #:checker 'blank-line-in-body #:expect '((4 3))))

    (test "reports a blank line between the forms of an Emacs Lisp defun"
      (assert-warnings
       #:source "(defun f (x)\n  (first x)\n\n  (second x))\n"
       #:dialect 'elisp
       #:checker 'blank-line-in-body #:expect '((4 3))))

    (test "accepts a blank line between top-level forms"
      (assert-warnings
       #:source "(define a 1)\n\n(define b 2)\n"
       #:checker 'blank-line-in-body #:expect '()))

    (test "accepts a blank line between the bindings of a `let'"
      (assert-warnings
       #:source "(let ((a 1)\n\n      (b 2))\n  (+ a b))\n"
       #:checker 'blank-line-in-body #:expect '()))

    (test "reports a run of blank lines as one finding"
      (assert-warnings
       #:source "(define (f x)\n  (first x)\n\n\n  (second x))\n"
       #:checker 'blank-line-in-body #:expect '((5 3))))))
