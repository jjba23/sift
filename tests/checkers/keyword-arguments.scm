;; sift --- checker: keyword-arguments
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Past five positional arguments a call stops being readable at the call
;; site: the reader has to look up the definition to know what the sixth one
;; means.  Lisp already has the answer -- keyword arguments -- and this rule
;; points at the moment to use it.
;;
;; The rule stays out of special forms and out of the calls that are meant to
;; be variadic (`list`, `format`, `+`), and the limit is configuration.

(define-module (tests checkers keyword-arguments)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: keyword-arguments"

    (test "reports a call with too many positional arguments"
      (assert-warnings #:source "(make-window a b c d e f g)\n"
                       #:checker 'keyword-arguments #:expect '((1 1))))

    (test "accepts a call with a readable number of arguments"
      (assert-warnings #:source "(make-window a b c)\n"
                       #:checker 'keyword-arguments #:expect '()))

    (test "accepts arguments that are already keywords"
      (assert-warnings #:source "(make-window a b :width 10 :height 20 :title \"x\" :icon i :modal m)\n"
                       #:checker 'keyword-arguments #:expect '()))

    (test "accepts the calls that are meant to be variadic"
      (assert-warnings #:source "(format #t \"~a ~a ~a ~a ~a ~a\" 1 2 3 4 5 6)\n"
                       #:checker 'keyword-arguments #:expect '()))

    (test "accepts a special form"
      (assert-warnings #:source "(let ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6)) a)\n"
                       #:checker 'keyword-arguments #:expect '()))

    ;; A declaration is not a call.  `(use-modules (a) (b) (c) (d) (e) (f))'
    ;; names six libraries and `#:export (a b c d e f)' names six exports, and
    ;; neither is a call anybody could shorten by passing keywords.
    (test "a use-modules form is a declaration, not a call"
      (assert-warnings
       #:source "(use-modules (srfi srfi-1) (srfi srfi-13) (ice-9 format)\n             (ice-9 match) (sift records) (sift reader))\n"
       #:checker 'keyword-arguments #:expect '()))

    (test "the export list of a define-module is data"
      (assert-warnings
       #:source "(define-module (demo thing)\n  #:use-module (srfi srfi-1)\n  #:export (one two three four five six))\n"
       #:checker 'keyword-arguments #:expect '()))

    (test "a Common Lisp package definition is a declaration"
      (assert-warnings
       #:source "(defpackage :demo\n  (:use :cl :alexandria)\n  (:export #:one #:two #:three #:four #:five #:six))\n"
       #:checker 'keyword-arguments #:dialect 'cl #:expect '()))

    (test "require and provide are declarations"
      (assert-warnings
       #:source "(require 'cl-lib)\n(provide 'demo)\n"
       #:checker 'keyword-arguments #:dialect 'elisp #:expect '()))

    (test "a parameter list is not a call"
      (assert-warnings
       #:source "(defun demo-f (a b c d e f g)\n  (list a b c d e f g))\n"
       #:checker 'keyword-arguments #:dialect 'elisp #:expect '()))

    (test "the limit is configuration"
      (assert-warnings #:source "(make-window a b c)\n" #:checker 'keyword-arguments
                       #:options '((max-positional . 2)) #:expect '((1 1))))))
