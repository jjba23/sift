;; sift --- checker: docstring-texinfo
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Guile docstrings are not plain text: `C-h f', `(help foo)' and the manual
;; render them as Texinfo, so an argument written `x' stays `x' and one
;; written `@var{x}' is typeset as a metavariable.  The rule asks each
;; parameter to be named with `@var{}'.

(define-module (tests checkers docstring-texinfo)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: docstring-texinfo"

    (test "reports a parameter the docstring does not document"
      (assert-warnings #:source "(define (f x)\n  \"Return a thing.\"\n  x)\n"
                       #:checker 'docstring-texinfo #:expect '((2 3))))

    (test "reports each undocumented parameter"
      (assert-warnings
       #:source "(define (f x y)\n  \"Return @var{x} plus something.\"\n  x)\n"
       #:checker 'docstring-texinfo #:expect '((2 3))))

    (test "accepts a docstring that documents every parameter"
      (assert-warnings
       #:source "(define (f x y)\n  \"Return @var{x} plus @var{y}.\"\n  x)\n"
       #:checker 'docstring-texinfo #:expect '()))

    (test "says nothing about a procedure with no parameters"
      (assert-warnings #:source "(define (f)\n  \"Return a thing.\"\n  1)\n"
                       #:checker 'docstring-texinfo #:expect '()))

    (test "says nothing about a definition with no docstring"
      (assert-warnings #:source "(define (f x) x)\n" #:checker 'docstring-texinfo
                       #:expect '()))))
