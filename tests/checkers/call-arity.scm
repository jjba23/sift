;; sift --- checker: call-arity
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A call that passes fewer arguments than the definition in the same file
;; requires is a call that will raise.  It is `info` because sift reads one
;; file: the definition a call means can live in another one, and a finding
;; that says "in this file" must not read as an error about the program.

(define-module (tests checkers call-arity)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: call-arity"

    (test "reports a call with too few arguments"
      (assert-warning-count
       #:source "(define (area width height) 0)\n(area 2)\n"
       #:checker 'call-arity
       #:count 1))

    (test "accepts a call with every argument"
      (assert-warnings #:source "(define (area width height) 0)\n(area 2 3)\n"
                       #:checker 'call-arity
                       #:expect '()))

    (test "optional, keyword and rest parameters are not required"
      (assert-warnings
       #:source "(define* (make-window parent #:optional width #:key height) 0)\n(make-window root)\n"
       #:checker 'call-arity
       #:expect '()))

    (test "a binding of the same name is not the definition"
      (assert-warnings
       #:source "(define (area width height) 0)\n(let ((area (lambda (x) x))) (area 1))\n"
       #:checker 'call-arity
       #:expect '()))

    (test "a procedure bound in a let is counted, not the top-level one"
      (assert-warning-count
       #:source "(define (area width height) 0)\n(let ((area (lambda (x) x))) (area 1 2))\n"
       #:checker 'call-arity
       #:count 1))

    (test "reports a call with too many arguments"
      (assert-warning-count
       #:source "(define (area width height) 0)\n(area 1 2 3)\n"
       #:checker 'call-arity
       #:count 1))

    (test "optional parameters raise the ceiling"
      (assert-warnings
       #:source "(define* (make-window parent #:optional width) 0)\n(make-window root 1)\n"
       #:checker 'call-arity
       #:expect '())
      (assert-warning-count
       #:source "(define* (make-window parent #:optional width) 0)\n(make-window root 1 2 3)\n"
       #:checker 'call-arity
       #:count 1))

    (test "a rest parameter takes as many arguments as a caller has"
      (assert-warnings
       #:source "(define (area width . rest) 0)\n(area 1 2 3)\n"
       #:checker 'call-arity
       #:expect '()))

    (test "reads the ceiling of a Common Lisp definition too"
      (assert-warning-count
       #:source "(defun area (width) 0)\n(area 1 2)\n"
       #:checker 'call-arity
       #:dialect 'cl
       #:count 1))

    (test "a name defined twice with two arities is left alone"
      (assert-warnings
       #:source "(define (f a) a)\n(define (f a b) b)\n(f)\n"
       #:checker 'call-arity
       #:expect '()))

    (test "reads a Common Lisp definition too"
      (assert-warning-count
       #:source "(defun area (width height) 0)\n(area 2)\n"
       #:checker 'call-arity
       #:dialect 'cl
       #:count 1))

    (test "a call inside a quote is data"
      (assert-warnings
       #:source "(define (area width height) 0)\n'(area 2)\n"
       #:checker 'call-arity
       #:expect '()))))
