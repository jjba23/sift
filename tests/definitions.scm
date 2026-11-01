;; sift --- definitions, bindings and scopes
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A jump is only as good as the scope it reads: the nearest binding is the
;; answer, and a name bound twice must not answer with the wrong one.  These
;; tests state that for the forms that bind names -- parameters, `let', an
;; internal definition, a named `let' -- and for the one place a Scheme file
;; binds a name without defining it, a `syntax-rules' pattern, whose variables
;; are what the template refers to and where an editor has nowhere else to
;; jump.
;;
;; The tests read real source text and ask about a character offset, because
;; that is what the language server asks: a test written against the tree
;; alone would pass on a resolver whose offsets are wrong.  The answer is
;; given as the name and the line and column where it is written, which is
;; what a reader sees when the editor jumps there.

(define-module (tests definitions)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift definitions)
  #:export (spec))

;;; ===== Reading and addressing =====

(define* (parse text #:key (dialect 'scheme))
  (call-with-values (lambda () (read-source #:source text #:dialect dialect))
    (lambda (ast errors) ast)))

(define (nth-occurrence text needle nth)
  "Return the character index of the @var{nth} occurrence of @var{needle}."
  (let loop ((start 0) (n 0))
    (let ((at (string-contains text needle start)))
      (cond
        [(not at) #f]
        [(= n nth) at]
        [else (loop (+ at 1) (+ n 1))]))))

(define* (where-at text needle #:key (nth 0) (delta 0) (dialect 'scheme))
  "Return the definition the @var{nth} occurrence of @var{needle} names,
as @code{(name line column)}, or @code{#f}."
  (let ((index (nth-occurrence text needle nth)))
    (and index
      (let ((definition (resolve-definition (parse text #:dialect dialect)
                                            (+ index delta))))
        (and definition
          (let ((location (node-start (definition-node definition))))
            (list (definition-name definition)
                  (source-location-line location)
                  (source-location-column location))))))))

(define-syntax-rule (assert-resolves text needle expected option ...)
  "Assert that a use site resolves to a @code{(name line column)}."
  (assert-equal #:expect expected #:got (where-at text needle option ...)))

;;; ===== Scope =====

(define (spec)
  (suite "definitions: scope and bindings"

    (test "a parameter is nearer than the definition of the same name"
      (assert-resolves "(define (f x)\n  x)\n"
                       "x)" (list "x" 1 12)))

    (test "a let binding wins over the definition it shadows"
      (assert-resolves "(define (f x)\n  (let ((x 1)) (g x)))\n"
                       "x)))" (list "x" 2 10)))

    (test "a named let binds its own name over the body"
      (assert-resolves "(define (count-down n)\n  (let loop ((n n))\n    (loop (- n 1))))\n"
                       "loop (-" (list "loop" 2 8)))

    (test "an internal definition is a definition of the body it sits in"
      (assert-resolves "(define (f)\n  (define (g) 1)\n  (g))\n"
                       "(g))" (list "g" 2 12) #:delta 1))

    (test "the name of a definition is a definition of itself"
      (assert-resolves "(define (f x) x)\n" "f x" (list "f" 1 10)))

    ;; ===== macro patterns =====

    (test "a template variable is defined by its pattern"
      (assert-resolves
       "(define-syntax swap!\n  (syntax-rules ()\n    ((_ a b)\n     (list a b))))\n"
       "a b)))" (list "a" 3 9)))

    (test "a pattern variable inside a nested list is still a variable"
      (assert-resolves
       "(define-syntax pair-of\n  (syntax-rules ()\n    ((_ (a b)) (list a b))))\n"
       "a b)" (list "a" 3 10) #:nth 1))

    (test "a literal in the pattern binds nothing"
      (assert-resolves
       "(define-syntax my-or\n  (syntax-rules (else)\n    ((_ (else x)) x)\n    ((_ a b) (if a a b))))\n"
       "else x))" #f))

    (test "an underscore binds nothing"
      (assert-resolves
       "(define-syntax ignore\n  (syntax-rules ()\n    ((_ _ x) x)))\n"
       "_ x)" #f))

    (test "the ellipsis binds nothing"
      (assert-resolves
       "(define-syntax all\n  (syntax-rules ()\n    ((_ x ...) (list x ...))))\n"
       "..." #f))

    (test "a renamed ellipsis binds nothing either"
      (assert-resolves
       "(define-syntax all2\n  (syntax-rules ::: ()\n    ((_ x :::) (list x :::))))\n"
       ":::" #f #:nth 1))

    (test "a binding inside a template still wins over the pattern"
      (assert-resolves
       "(define-syntax swap!\n  (syntax-rules ()\n    ((_ a b)\n     (let ((tmp a)) (set! b tmp)))))\n"
       "tmp)))" (list "tmp" 4 13)))

    (test "let-syntax binds its keyword over the body"
      (assert-resolves
       "(let-syntax ((with-two (syntax-rules () ((_ form) (list form form)))))\n  (with-two 1))\n"
       "with-two 1" (list "with-two" 1 15)))

    (test "letrec-syntax binds its keyword over the body too"
      (assert-resolves
       "(letrec-syntax ((with-two (syntax-rules () ((_ form) (list form form)))))\n  (with-two 1))\n"
       "with-two 1" (list "with-two" 1 18)))

    (test "a syntax-case clause binds its pattern variables"
      (assert-resolves
       "(define (f x)\n  (syntax-case x ()\n    ((a b) (list a b))))\n"
       "a b)" (list "a" 3 7) #:nth 1))

    ;; ===== what the outline sees =====

    (test "a define-syntax is a macro in the outline"
      (let ((definitions
             (parse-definitions
              (parse "(define-syntax swap!\n  (syntax-rules () ((_ a b) (list a b))))\n"))))
        (assert-equal #:expect (list (list "swap!" 'macro))
          #:got (map (lambda (definition)
                       (list (definition-name definition)
                             (definition-kind definition)))
                     definitions))))

    (test "a pattern variable is not a definition of the file"
      (assert-equal #:expect 1
        #:got (length
               (parse-definitions
                (parse "(define-syntax swap!\n  (syntax-rules () ((_ a b) (list a b))))\n")))))))
