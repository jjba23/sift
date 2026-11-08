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
                       #:options '((max-positional . 2)) #:expect '((1 1))))

    ;; Emacs Lisp states several bindings in one form -- `(setq a 1 b 2)' --
    ;; and that is idiomatic however many it states: what a reader counts is
    ;; the declarations, not the arguments, so a `setq' of nine bindings is an
    ;; ordinary thing to write where a call of nine arguments is not.
    (test "a setq of many bindings is not a call with too many arguments"
      (assert-warnings
       #:source "(setq a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9)\n"
       #:checker 'keyword-arguments #:dialect 'elisp #:expect '()))

    (test "a setopt of many declarations is not a call with too many arguments"
      (assert-warnings
       #:source "(setopt a 1 b 2 c 3 d 4 e 5 f 6 g 7)\n"
       #:checker 'keyword-arguments #:dialect 'elisp #:expect '()))

    (test "the declarations of a pair form are counted, so a long one is still reported"
      (assert-warnings
       #:source "(setq a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10 k 11)\n"
       #:checker 'keyword-arguments #:dialect 'elisp #:expect '((1 1))))

    (test "the limit of a pair form is configuration too"
      (assert-warnings #:source "(setq a 1 b 2 c 3)\n" #:checker 'keyword-arguments
                       #:dialect 'elisp #:options '((max-positional . 1))
                       #:expect '((1 1))))

    ;; A record macro takes one argument per field, so its argument count is
    ;; the size of the record and not the length of a parameter list.
    ;; `(package :name "x" :version "1")' is not Guix, and a rule that asked
    ;; for it would be asking for code no Guix command reads.
    (test "a Guix package is a record, not a call with too many arguments"
      (assert-warnings
       #:source "(package\n  (name \"x\")\n  (version \"1\")\n  (source s)\n  (build-system gnu-build-system)\n  (home-page \"h\")\n  (synopsis \"s\"))\n"
       #:checker 'keyword-arguments #:expect '()))

    (test "an operating-system is a record too"
      (assert-warnings
       #:source "(operating-system\n  (host-name \"h\")\n  (timezone \"Europe/Amsterdam\")\n  (locale \"en_US.utf8\")\n  (bootloader b)\n  (file-systems fs)\n  (users users))\n"
       #:checker 'keyword-arguments #:expect '()))

    (test "the fields of a record macro are still linted"
      ;; The exemption is about the *count*: a package whose fields are wrong
      ;; in some other way is still a package sift has an opinion about.
      (assert-warnings
       #:source "(package\n  (name \"x\")\n  (version \"1\")\n  (source (if a b))\n  (build-system gnu-build-system)\n  (home-page \"h\")\n  (synopsis \"s\"))\n"
       #:checker 'if-without-else #:expect '((4 12))))

    (test "a project names its own record macros"
      (assert-warnings
       #:source "(my-record one two three four five six seven)\n"
       #:checker 'keyword-arguments
       #:options '((field-macros . (my-record))) #:expect '()))))

