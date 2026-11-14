;; sift --- reader tests, Racket
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Racket is a Scheme in everything the reader already understands -- `#;',
;; `#|...|#', a bytevector, a square bracket, `#'`/`#``/`#,`/`#,` -- and a
;; language of its own in three places: a `#lang' line opens the file, `#:kw'
;; is a keyword rather than a symbol, and Guile's `#{...}#' is not there at
;; all.  What this file pins down is both halves: the syntax that is shared is
;; read without a warning, the syntax that is another dialect's is reported,
;; and a Racket file round-trips byte for byte.

(define-module (tests reader-racket)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (tests helpers)
  #:export (spec))

(define (round-trip source)
  (call-with-values (lambda () (read-source-string source #:dialect 'racket))
    (lambda (ast errors) (ast->text ast))))

(define (messages source)
  "Return what the reader says about @var{source}, read as Racket."
  (call-with-values (lambda () (read-source-string source #:dialect 'racket))
    (lambda (ast errors) (map lint-warning-message errors))))

(define (messages-as dialect source)
  (call-with-values (lambda () (read-source-string source #:dialect dialect))
    (lambda (ast errors) (map lint-warning-message errors))))

(define (data-nodes source)
  (call-with-values (lambda () (read-source-string source #:dialect 'racket))
    (lambda (ast errors) (filter node-data? ast))))

(define (spec)
  (suite "reader: Racket"

    (test "a Racket file is one file of each of its four extensions"
      (map (lambda (path)
             (assert-equal #:expect 'racket #:got (detect-dialect #:file path)
               #:name path))
           '("guestbook.rkt" "data.rktd" "helper.rktl" "manual.scrbl")))

    (test "a `#lang' line and a `racket' shebang name the dialect"
      (assert-equal #:expect 'racket
        #:got (detect-dialect #:source "#lang racket\n(provide x)\n"))
      (assert-equal #:expect 'racket
        #:got (detect-dialect #:source "#!/usr/bin/env racket\n(provide x)\n"))
      ;; the extension is what the user chose, and it is asked first
      (assert-equal #:expect 'scheme
        #:got (detect-dialect #:file "guestbook.scm"
                              #:source "#lang racket\n")))

    (test "the `#lang' line is one node, and it is data rather than trivia"
      (let ((node (car (data-nodes "#lang racket/base\n(provide x)\n"))))
        (assert-true (node-language? node) #:name "a language node")
        (assert-equal #:expect "#lang racket/base" #:got (node-value node))))

    (test "the whole line is kept, the language path included"
      (assert-same-text #:expect "#lang typed/racket/base\n(provide x)\n"
                        #:got (round-trip "#lang typed/racket/base\n(provide x)\n")))

    (test "a `#lang' line in another dialect is reported, and kept"
      (assert-equal #:expect '("a `#lang' line is Racket syntax, not Scheme syntax")
        #:got (map (lambda (message) message)
                   (messages-as 'scheme "#lang racket\n(define x 1)\n"))))

    (test "a keyword is an atom of its own type"
      (let ((node (car (data-nodes "#:height"))))
        (assert-equal #:expect 'keyword #:got (node-atom-type node))
        (assert-equal #:expect "#:height" #:got (node-value node))))

    (test "the reader macros Racket shares with Scheme are not reported"
      (assert-equal #:expect '()
        #:got (messages "#;(dead)\n#| note |#\n#u8(1 2)\n#\\a\n#'x\n#`x\n#,x\n#,@x\n")))

    (test "Guile's extended symbol is not Racket syntax, and says so"
      (let ((reported (messages "#{foo bar}#\n")))
        (assert-equal #:expect 1 #:got (length reported))
        (assert-equal #:expect 'warning
          #:got (call-with-values
                  (lambda () (read-source-string "#{foo bar}#\n"
                                                 #:dialect 'racket))
                  (lambda (ast errors) (lint-warning-severity (car errors)))))
        (assert-equal #:expect #t #:got (and (string-contains (car reported) "Guile") #t)
          #:name "the message names the dialect it belongs to")))

    (test "a square bracket is a list here, and not a vector"
      (let ((clause (cadr (node-elements (car (data-nodes "(cond [else 1])"))))))
        (assert-false (node-vector? clause) #:name "a Racket clause bracket")))

    (test "Racket's own read macros keep the datum glued to them"
      (assert-same-text #:expect "#hash((a . 1))\n#s(posn 1 2)\n#rx\"[a-z]+\"\n#&box\n"
                        #:got (round-trip "#hash((a . 1))\n#s(posn 1 2)\n#rx\"[a-z]+\"\n#&box\n"))
      (let ((node (last (data-nodes "#rx\"[a-z]+\"\n"))))
        (assert-equal #:expect "#rx" #:got (node-token node))
        (assert-equal #:expect "#rx\"[a-z]+\"" #:got (node->string node))))

    (test "a whole Racket file round-trips byte for byte"
      (let ((text (slurp (string-append (test-root)
                                        "/tests/fixtures/racket/community/expected/base.rkt"))))
        (assert-same-text #:expect text #:got (round-trip text))))))
