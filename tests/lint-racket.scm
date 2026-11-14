;; sift --- lint tests, Racket
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Racket does not get every rule, and this file is where that is stated rather
;; than implied.  The rules that read a form every Lisp shares -- `cond',
;; `case', `let', `apply`, `quote -- run on a Racket file; the ones whose
;; premise is another dialect's are left out, because a finding a reader has to
;; argue with is worse than no finding at all:
;;
;;   `unused-import', `library-not-found' and `unused-definition' read
;;   `import', `define-library' and `define-module', and Racket spells all
;;   three with `require' and `provide';
;;   `call-arity` reads a parameter list whose optionals and keywords are
;;   spelled `&optional' and `#:key', and Racket spells them `[id default]';
;;   `case-literal-datum` says `case' compares with `eqv?', and Racket's
;;   `case' compares with `equal?', so a string datum there matches;
;;   `negated-if` repairs `(if (not x) a)' into `(unless x a)', which loses the
;;   else branch of an `if` that Racket requires to have two;
;;   `docstring-texinfo`, `redundant-progn` and `case-literal-datum` name forms
;;   and conventions Racket does not have; and `internal-define` is Scheme's,
;;   because Racket allows a definition interleaved with expressions in a
;;   body.
;;
;; The corpus file is the other half of the claim: what an idiomatic Racket
;; module of this project's own style looks like reports nothing.

(define-module (tests lint-racket)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (racket-rule-names)
  (map lint-checker-name (checkers-for-dialect 'racket)))

(define (messages-of source checker)
  (map lint-warning-message
       (warnings-of source #:dialect 'racket #:checker checker)))

(define (spec)
  (suite "lint: Racket"

    (test "the corpus file has nothing to report"
      (assert-equal #:expect '()
        #:got (findings-of (lint-file (string-append
                                       (test-root)
                                       "/tests/fixtures/racket/community/expected/base.rkt")
                                      #:dialect 'racket))))

    (test "the rules about a form every Lisp shares run on Racket"
      (let ((names (racket-rule-names)))
        (for-each (lambda (name)
                    (assert-equal #:expect #t #:got (and (memq name names) #t)
                      #:name (symbol->string name)))
                  '(if-without-else empty-form eta-reduction
                                    cond-else-not-last let-sibling apply-of-list
                                    quasiquote-dotted-unquote unused-binding
                                    composition beta-reduction let-simplification
                                    naming-convention trailing-whitespace))))

    (test "the rules whose premise is another dialect's do not"
      (let ((names (racket-rule-names)))
        (for-each (lambda (name)
                    (assert-equal #:expect #f #:got (and (memq name names) #t)
                      #:name (symbol->string name)))
                  '(unused-import library-not-found unused-definition
                                  call-arity case-literal-datum negated-if
                                  docstring-texinfo package-synopsis
                                  defun-no-docstring lexical-binding-missing
                                  dangling-paren))))

    (test "an `if` with one arm is reported, and `when` is the repair"
      (assert-warnings #:source "(if (ready?)\n    (go))\n"
                       #:dialect 'racket
                       #:checker 'if-without-else #:expect '((1 2)))
      (assert-fixes-to-nothing #:source "(if (ready?)\n    (go))\n"
                               #:dialect 'racket
                               #:checker 'if-without-else))

    (test "a top-level comment takes two semicolons, and three is a heading"
      (assert-warnings #:source "; one is for the margin\n(define x 1)\n"
                       #:dialect 'racket
                       #:checker 'comment-semicolons #:expect '((1 1)))
      (assert-warnings #:source ";; a comment about the form under it\n(define x 1)\n"
                       #:dialect 'racket
                       #:checker 'comment-semicolons #:expect '())
      (assert-warnings #:source ";;; a section\n(define x 1)\n"
                       #:dialect 'racket
                       #:checker 'comment-semicolons #:expect '()))

    (test "a call that states too much is told to consider a Racket keyword"
      (let ((messages (messages-of "(make-window a b c d e f g)\n"
                                   'keyword-arguments)))
        (assert-equal #:expect 1 #:got (length messages))
        (assert-equal #:expect #t
          #:got (and (string-contains (car messages) "#:key") #t)
          #:name "the keyword is spelled the Racket way")))

    (test "a `match` pattern is a pattern, not a call"
      (assert-warnings #:source "(match value\n  [(apply list xs) 1]\n  [_ 2])\n"
                       #:dialect 'racket
                       #:checker 'apply-of-list #:expect '()))

    (test "`letrec` with one binding is reported, and the body is not read as a call"
      (assert-warnings #:source "(letrec ([x 1]) (+ x 1))\n"
                       #:dialect 'racket
                       #:checker 'letrec-single-binding #:expect '((1 1))))))
