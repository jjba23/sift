;; sift --- checker: practices
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; One suite for the handful of rules that say a form reads as safe and is not
;; quite: each is silent where it should be as well as loud where it should be,
;; because the false positive is what makes a reader turn a rule off.

(define-module (tests checkers practices)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: practices"

    (test "a quoted number is reported and the quote is removable"
      (assert-warning-count #:source "'5\n" #:checker 'quote-of-constant #:count 1)
      (assert-fix #:source "'5\n" #:checker 'quote-of-constant #:expected "5\n")
      (assert-fixes-to-nothing #:source "'5\n" #:checker 'quote-of-constant))

    (test "a quoted string, character and list are told apart"
      (assert-warning-count #:source "'\"text\"\n" #:checker 'quote-of-constant
                            #:count 1)
      (assert-warning-count #:source "'#\\a\n" #:checker 'quote-of-constant
                            #:count 1)
      ;; the empty list is the one datum a quote still writes
      (assert-warnings #:source "'()\n" #:checker 'quote-of-constant #:expect '()))

    (test "a quote around a symbol is left alone"
      (assert-warnings #:source "'(a b)\n" #:checker 'quote-of-constant
                       #:expect '()))

    (test "eq? on a number and on a string are reported"
      (assert-warning-count #:source "(eq? (length items) 3)\n"
                            #:checker 'eq-on-literal #:count 1)
      (assert-warning-count #:source "(eq? name \"x\")\n"
                            #:checker 'eq-on-literal #:count 1))

    (test "eq? on two variables and eqv? on a number are silent"
      (assert-warnings #:source "(eq? a b)\n" #:checker 'eq-on-literal
                       #:expect '())
      (assert-warnings #:source "(eqv? a 3)\n" #:checker 'eq-on-literal
                       #:expect '()))

    (test "a cond with clauses after else is reported"
      (assert-warning-count
       #:source "(cond (a 1) (else 2) (b 3))\n"
       #:checker 'cond-else-not-last #:count 1)
      (assert-warnings
       #:source "(cond (a 1) (else 2))\n"
       #:checker 'cond-else-not-last #:expect '()))

    (test "a case datum listed twice is reported"
      (assert-warning-count
       #:source "(case x ((a) 1) ((a b) 2) (else 3))\n"
       #:checker 'duplicate-case-datum #:count 1)
      (assert-warnings
       #:source "(case x ((a) 1) ((b) 2) (else 3))\n"
       #:checker 'duplicate-case-datum #:expect '()))

    (test "six parameters are reported and five are not"
      (assert-warning-count #:source "(define (f a b c d e g) 1)\n"
                            #:checker 'too-many-arguments #:count 1)
      (assert-warnings #:source "(define (f a b c d e) 1)\n"
                       #:checker 'too-many-arguments #:expect '())
      ;; a define of a value has no parameters to count
      (assert-warnings #:source "(define x 5)\n"
                       #:checker 'too-many-arguments #:expect '()))

    (test "a defcustom without :type is reported in Emacs Lisp"
      (assert-warning-count
       #:source "(defcustom sift-thing nil\n  \"A thing.\"\n  :group 'sift)\n"
       #:checker 'defcustom-without-type #:dialect 'elisp #:count 1)
      (assert-warnings
       #:source "(defcustom sift-thing nil\n  \"A thing.\"\n  :type 'boolean)\n"
       #:checker 'defcustom-without-type #:dialect 'elisp #:expect '()))))
