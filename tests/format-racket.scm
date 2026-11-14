;; sift --- format tests, Racket
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; What Racket's indentation is, stated as output.  The differences from Scheme
;; are the point of the file: a body goes two columns in from its opener, `if`
;; lines its arms up under the test rather than at two, and the clauses of a
;; `cond', a `case' and a `match' are written with square brackets.
;;
;; The corpus is walked first, exactly as it is for the other dialects: every
;; file under tests/fixtures/racket formats to its expected file, and every
;; expected file is a fixed point of formatting.

(define-module (tests format-racket)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift format)
  #:use-module (tests helpers)
  #:export (spec))

(define (racket-pairs)
  (filter (lambda (pair)
            (and (eq? (dialect-of (car pair)) 'racket)
              (eq? (style-of (car pair)) 'community)))
          (fixture-pairs)))

(define (fmt source)
  (format-string source #:dialect 'racket))

(define (spec)
  (suite "format: Racket"

    (test "every fixture formats to its expected file"
      (for-each assert-fixture (racket-pairs)))

    (test "every expected file is a fixed point of formatting"
      (for-each assert-fixture-idempotent (racket-pairs)))

    (test "a definition's body indents two columns"
      (assert-same-text #:expect "(define (area width height)\n  (* width height))\n"
                        #:got (fmt "(define (area width height)\n      (* width height))\n"))
      (assert-same-text #:expect "(define answer\n  42)\n"
                        #:got (fmt "(define answer\n42)\n")))

    (test "`if` lines its arms up under the test, where every Racket file puts them"
      (assert-same-text #:expect "(if (ready?)\n    (go)\n    (wait))\n"
                        #:got (fmt "(if (ready?)\n(go)\n(wait))\n")))

    (test "a `let` body and a `for` body indent two columns"
      (assert-same-text
       #:expect "(let ([total 0])\n  (for ([x (in-list xs)])\n    (+ total x)))\n"
       #:got (fmt "(let ([total 0])\n(for ([x (in-list xs)])\n(+ total x)))\n")))

    (test "the clauses of a `cond` become square brackets"
      (assert-same-text
       #:expect "(cond\n  [(empty? xs) #f]\n  [else (first xs)])\n"
       #:got (fmt "(cond\n((empty? xs) #f)\n(else (first xs)))\n")))

    (test "a `match` puts its clauses two columns in, and brackets them"
      (assert-same-text
       #:expect "(match value\n  [(list a b) (+ a b)]\n  [_ #f])\n"
       #:got (fmt "(match value\n((list a b) (+ a b))\n(_ #f))\n")))

    (test "a `match` subject is an argument, and keeps its own delimiters"
      ;; the clause style belongs to the clauses, not to the call around the
      ;; subject: bracketing it would change the program
      (assert-same-text
       #:expect "(match (build-value a\n                    b)\n  [(list a b) (+ a b)])\n"
       #:got (fmt "(match (build-value a\n b)\n((list a b) (+ a b)))\n")))

    (test "a `syntax-case` literals list is not a clause"
      (assert-same-text
       #:expect "(syntax-case stx (a b)\n  [(_ x) #'x])\n"
       #:got (fmt "(syntax-case stx (a b)\n((_ x) #'x))\n")))

    (test "a project that writes parentheses keeps them"
      (assert-same-text
       #:expect "(cond\n  ((empty? xs) #f)\n  (else 1))\n"
       #:got (format-string "(cond\n  ((empty? xs) #f)\n  (else 1))\n"
                            #:dialect 'racket #:brackets 'parens)))

    (test "the `#lang` line stays at the margin, and the form under it at two"
      (assert-same-text #:expect "#lang racket\n\n(define x\n  1)\n"
                        #:got (fmt "#lang racket\n\n(define x\n1)\n")))

    (test "a keyword is one atom, and is never respelled"
      (assert-same-text #:expect "(struct posn (x y)\n  #:transparent)\n"
                        #:got (fmt "(struct posn (x y)\n#:transparent)\n")))

    (test "a one-blank-line project separates two top-level forms"
      (assert-same-text
       #:expect "#lang racket\n\n(require racket/list)\n\n(provide x)\n"
       #:got (format-string "#lang racket\n(require racket/list)\n(provide x)\n"
                            #:dialect 'racket #:blank-lines 'one)))

    (test "formatting a Racket file twice changes nothing"
      (assert-idempotent #:dialect 'racket
                         #:source "(define (f x)\n(cond\n((= x 1) 'one)\n(else 'other)))\n"))))
