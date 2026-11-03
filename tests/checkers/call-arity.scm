;; sift --- checker: call-arity
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A call whose arguments do not match the definition in the same file is a
;; call that will raise, and what a parameter list accepts is read from its
;; markers: `#:optional' and `&optional' are arguments a caller may pass
;; positionally, `#:key' and `&key' are arguments it passes by keyword,
;; `#:rest', `&rest', `&body' and a dotted tail take as many as it has, and
;; `&aux' binds nothing it passes.
;;
;; It is `info' because sift reads one file: the definition a call means can
;; live in another one, and a finding that says "in this file" must not read
;; as an error about the program.

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

    (test "reports a call with too many arguments"
      (assert-warning-count
       #:source "(define (area width height) 0)\n(area 1 2 3)\n"
       #:checker 'call-arity
       #:count 1))

    (test "counts a definition whose value is a lambda"
      (assert-warning-count
       #:source "(define area (lambda (width height) 0))\n(area 1)\n"
       #:checker 'call-arity
       #:count 1)
      (assert-warnings
       #:source "(define area (lambda (width height) 0))\n(area 1 2)\n"
       #:checker 'call-arity
       #:expect '()))

    (test "optional, keyword and rest parameters are not required"
      (assert-warnings
       #:source "(define* (make-window parent #:optional width #:key height) 0)\n(make-window root)\n"
       #:checker 'call-arity
       #:expect '()))

    (test "an optional parameter raises the ceiling"
      (assert-warnings
       #:source "(define* (make-window parent #:optional width) 0)\n(make-window root 1)\n"
       #:checker 'call-arity
       #:expect '())
      (assert-warning-count
       #:source "(define* (make-window parent #:optional width) 0)\n(make-window root 1 2 3)\n"
       #:checker 'call-arity
       #:count 1))

    (test "an optional parameter with a default is still one argument"
      (assert-warnings
       #:source "(define* (make-window parent #:optional (width 1)) 0)\n(make-window root)\n(make-window root 2)\n"
       #:checker 'call-arity
       #:expect '())
      (assert-warning-count
       #:source "(define* (make-window parent #:optional (width 1)) 0)\n(make-window root 2 3)\n"
       #:checker 'call-arity
       #:count 1))

    (test "a key parameter is passed by keyword, not positionally"
      (assert-warnings
       #:source "(define* (make-window parent #:key height) 0)\n(make-window root #:height 3)\n"
       #:checker 'call-arity
       #:expect '())
      (assert-warning-count
       #:source "(define* (make-window parent #:key height) 0)\n(make-window root 3)\n"
       #:checker 'call-arity
       #:count 1))

    (test "reports a keyword the definition does not take"
      (assert-warning-count
       #:source "(define* (make-window parent #:key height) 0)\n(make-window root #:width 3)\n"
       #:checker 'call-arity
       #:count 1))

    (test "reports a keyword with no value"
      (assert-warning-count
       #:source "(define* (make-window parent #:key height) 0)\n(make-window root #:height)\n"
       #:checker 'call-arity
       #:count 1))

    (test "reports a keyword to a definition that takes none"
      (assert-warning-count
       #:source "(define (area width height) 0)\n(area 1 2 #:unit)\n"
       #:checker 'call-arity
       #:count 1))

    (test "allow-other-keys takes the keyword set away"
      (assert-warnings
       #:source "(define* (make-window parent #:key height #:allow-other-keys) 0)\n(make-window root #:width 3)\n"
       #:checker 'call-arity
       #:expect '()))

    (test "a rest parameter takes as many arguments as a caller has"
      (assert-warnings
       #:source "(define (area width . rest) 0)\n(area 1 2 3)\n"
       #:checker 'call-arity
       #:expect '())
      (assert-warnings
       #:source "(define* (area width #:rest rest) 0)\n(area 1 2 3 #:unit 4)\n"
       #:checker 'call-arity
       #:expect '()))

    (test "a rest parameter does not lower the floor"
      (assert-warning-count
       #:source "(define (area width . rest) 0)\n(area)\n"
       #:checker 'call-arity
       #:count 1))

    (test "a case-lambda is right when some clause accepts the call"
      (let ((source "(define handle (case-lambda (() 0) ((a) 1) ((a b) 2)))\n"))
        (assert-warnings
         #:source (string-append source "(handle)\n(handle 1)\n(handle 1 2)\n")
         #:checker 'call-arity
         #:expect '())
        (assert-warning-count
         #:source (string-append source "(handle 1 2 3)\n")
         #:checker 'call-arity
         #:count 1)))

    (test "a case-lambda clause list with a gap reports the counts it takes"
      (let ((source "(define handle (case-lambda ((a) 1) ((a b c) 3)))\n"))
        (assert-warnings
         #:source (string-append source "(handle 1)\n(handle 1 2 3)\n")
         #:checker 'call-arity
         #:expect '())
        (assert-warning-count
         #:source (string-append source "(handle 1 2)\n")
         #:checker 'call-arity
         #:count 1)))

    (test "reads the parameters of a Common Lisp definition"
      (assert-warning-count
       #:source "(defun area (width height) 0)\n(area 2)\n"
       #:checker 'call-arity
       #:dialect 'cl
       #:count 1)
      (assert-warning-count
       #:source "(defun area (width) 0)\n(area 1 2)\n"
       #:checker 'call-arity
       #:dialect 'cl
       #:count 1))

    (test "reads &optional, &rest, &key and &aux in Common Lisp"
      (assert-warnings
       #:source "(defun area (width &optional height) 0)\n(area 1)\n(area 1 2)\n"
       #:checker 'call-arity
       #:dialect 'cl
       #:expect '())
      (assert-warning-count
       #:source "(defun area (width &optional height) 0)\n(area 1 2 3)\n"
       #:checker 'call-arity
       #:dialect 'cl
       #:count 1)
      (assert-warnings
       #:source "(defun area (width &optional (height 1)) 0)\n(area 1)\n"
       #:checker 'call-arity
       #:dialect 'cl
       #:expect '())
      (assert-warnings
       #:source "(defun area (width &rest rest) 0)\n(area 1 2 3)\n"
       #:checker 'call-arity
       #:dialect 'cl
       #:expect '())
      (assert-warnings
       #:source "(defun area (width &key height) 0)\n(area 1 :height 2)\n"
       #:checker 'call-arity
       #:dialect 'cl
       #:expect '())
      (assert-warning-count
       #:source "(defun area (width &key height) 0)\n(area 1 2)\n"
       #:checker 'call-arity
       #:dialect 'cl
       #:count 1)
      (assert-warning-count
       #:source "(defun area (width &key height) 0)\n(area 1 :width 2)\n"
       #:checker 'call-arity
       #:dialect 'cl
       #:count 1)
      (assert-warnings
       #:source "(defun area (width &key height &allow-other-keys) 0)\n(area 1 :width 2)\n"
       #:checker 'call-arity
       #:dialect 'cl
       #:expect '())
      (assert-warning-count
       #:source "(defun area (width &aux (height 1)) 0)\n(area 1 2)\n"
       #:checker 'call-arity
       #:dialect 'cl
       #:count 1))

    (test "reads &body of a Common Lisp macro as a rest parameter"
      (assert-warnings
       #:source "(defmacro area (width &body body) 0)\n(area 1 2 3)\n"
       #:checker 'call-arity
       #:dialect 'cl
       #:expect '()))

    (test "reads &optional and &rest in Emacs Lisp"
      (assert-warnings
       #:source "(defun area (width &optional height) 0)\n(area 1)\n(area 1 2)\n"
       #:checker 'call-arity
       #:dialect 'elisp
       #:expect '())
      (assert-warning-count
       #:source "(defun area (width &optional height) 0)\n(area 1 2 3)\n"
       #:checker 'call-arity
       #:dialect 'elisp
       #:count 1)
      (assert-warnings
       #:source "(defun area (width &rest rest) 0)\n(area 1 2 3)\n"
       #:checker 'call-arity
       #:dialect 'elisp
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

    (test "a named let is a procedure whose parameters are its variables"
      (assert-warnings
       #:source "(let loop ((i 0) (j 1)) (loop 0 1))\n"
       #:checker 'call-arity
       #:expect '())
      (assert-warning-count
       #:source "(let loop ((i 0) (j 1)) (loop 0))\n"
       #:checker 'call-arity
       #:count 1))

    (test "a definition inside a body is the one a call in it means"
      (let ((source "(define (outer)\n  (define (inner a b) 0)\n  "))
        (assert-warnings
         #:source (string-append source "(inner 1 2))\n")
         #:checker 'call-arity
         #:expect '())
        (assert-warning-count
         #:source (string-append source "(inner 1))\n")
         #:checker 'call-arity
         #:count 1)))

    (test "a name defined twice accepts what either definition accepts"
      (let ((source "(define (f a) a)\n(define (f a b) b)\n"))
        (assert-warnings
         #:source (string-append source "(f 1)\n(f 1 2)\n")
         #:checker 'call-arity
         #:expect '())
        (assert-warning-count
         #:source (string-append source "(f)\n")
         #:checker 'call-arity
         #:count 1)
        (assert-warning-count
         #:source (string-append source "(f 1 2 3)\n")
         #:checker 'call-arity
         #:count 1)))

    (test "a call inside a quote is data"
      (assert-warnings
       #:source "(define (area width height) 0)\n'(area 2)\n"
       #:checker 'call-arity
       #:expect '()))))
