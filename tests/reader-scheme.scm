;; sift --- reader tests, Scheme
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; What the Scheme reader must understand and, just as importantly, what it
;; must keep.  Every test here is a statement about a node in the tree, and
;; the round-trip test at the end is the one that matters most: a file that is
;; read and written back must be the file that was read.

(define-module (tests reader-scheme)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (tests helpers)
  #:export (spec))

(define (round-trip source)
  (call-with-values (lambda () (read-source-string source))
    (lambda (ast errors) (ast->text ast))))

(define (kind-of-thing source)
  (call-with-values (lambda () (read-source-string source))
    (lambda (ast errors)
      (map (lambda (node) (node-atom-type node))
        (filter node-atom? ast)))))

(define (comment-texts source)
  (call-with-values (lambda () (read-source-string source))
    (lambda (ast errors)
      (let loop ((rest ast) (out '()))
        (cond ((null? rest) (reverse out))
          ((node-comment? (car rest))
           (loop (cdr rest) (cons (node-value (car rest)) out)))
          ((node-list? (car rest))
           (loop (append (node-children (car rest)) (cdr rest)) out))
          ((node-block-comment? (car rest))
           (loop (cdr rest) (cons (node-value (car rest)) out)))
          (else (loop (cdr rest) out)))))))

(define (inline-flags source)
  (call-with-values (lambda () (read-source-string source))
    (lambda (ast errors)
      (let loop ((rest ast) (out '()))
        (cond ((null? rest) (reverse out))
          ((node-comment? (car rest))
           (loop (cdr rest) (cons (node-inline? (car rest)) out)))
          ((node-list? (car rest))
           (loop (append (node-children (car rest)) (cdr rest)) out))
          (else (loop (cdr rest) out)))))))

(define (read-sexps-node source)
  (call-with-values (lambda () (read-source-string source))
    (lambda (ast errors) (filter node-data? ast))))

(define (spec)
  (suite "reader: Scheme"

    (test "top-level atoms keep their kind"
      (assert-equal #:expect '(symbol keyword number number string character)
        #:got (kind-of-thing "foo :key 42 3/4 \"s\" #\\a")))

    (test "a list is a list of its elements"
      (assert-equal #:expect '((a b (c d)))
        #:got (read-sexps "(a b (c d))")))

    (test "a dotted pair keeps its dot as an atom"
      (assert-same-text #:expect "(a . b)" #:got (round-trip "(a . b)"))
      (assert-equal #:expect '("a" "." "b")
        #:got (map (lambda (node) (node-value node))
                (node-elements (car (read-sexps-node "(a . b)"))))))

    (test "vectors and bytevectors are read with their own opener"
      (assert-equal #:expect '(#(1 2 3))
        #:got (read-sexps "#(1 2 3)"))
      (call-with-values (lambda () (read-source-string "#u8(1 2)"))
        (lambda (ast errors)
          (assert-equal #:expect "#u8(1 2)"
            #:got (ast->text ast)))))

    (test "every reader prefix applies to one datum"
      (assert-equal #:expect '((quote a) (quasiquote (b (unquote c) (unquote-splicing d))) (function e))
        #:got (read-sexps "'a `(b ,c ,@d) #'e")))

    (test "nested prefixes stay on the datum they belong to"
      (assert-equal #:expect '((quote #(1 2)))
        #:got (read-sexps "'#(1 2)"))
      (assert-equal #:expect '((quote (quasiquote (a (unquote b)))))
        #:got (read-sexps "'`(a ,b)")))

    (test "a prefix separated from its datum still reads"
      (assert-equal #:expect '((quote a))
        #:got (read-sexps "'  a")))

    (test "block comments nest, and none of them is lost"
      (assert-same-text #:expect "#| a #| b |# c |# x"
        #:got (round-trip "#| a #| b |# c |# x")))

    (test "a block comment between two elements is preserved"
      (assert-same-text #:expect "(a #| note |# b)"
        #:got (round-trip "(a #| note |# b)")))

    (test "a datum comment holds exactly one datum"
      (assert-equal #:expect '(a b)
        #:got (read-sexps "#;foo a b"))
      (assert-same-text #:expect "#;foo a"
        #:got (round-trip "#;foo a")))

    (test "the datum comment may hold a datum with a datum comment"
      (assert-same-text #:expect "#;#;a b c"
        #:got (round-trip "#;#;a b c")))

    (test "a datum comment inside a list comments out one element"
      (assert-equal #:expect '((a c))
        #:got (read-sexps "(a #;b c)")))

    (test "the reader marks everything inside a datum comment"
      (call-with-values (lambda () (read-source-string "#;(a (b c))"))
        (lambda (ast errors)
          (let ((inner (node-child (car ast))))
            (assert-true (node-commented-out? inner) #:name "the datum itself")
            (assert-true (node-commented-out?
                            (car (node-elements inner)))
              #:name "its children")))))

    (test "a shebang is a node, not a reader directive"
      (assert-same-text #:expect "#!/usr/bin/env guile\n(foo)\n"
        #:got (round-trip "#!/usr/bin/env guile\n(foo)\n")))

    (test "string escapes are kept exactly as they were written"
      (assert-same-text #:expect "(display \"a \\\" b\\n\\t\\x41;\")"
        #:got (round-trip "(display \"a \\\" b\\n\\t\\x41;\")")))

    (test "a backslash-newline inside a string is kept"
      (assert-same-text #:expect "\"one \\\ntwo\""
        #:got (round-trip "\"one \\\ntwo\"")))

    (test "character literals keep their spelling"
      (assert-equal #:expect '("#\\a" "#\\space" "#\\newline" "#\\x41")
        #:got (read-sexps "#\\a #\\space #\\newline #\\x41")))

    (test "numbers are not normalised"
      (assert-equal #:expect '(#x10 #b101 #e1.0 1/3 1e10)
        #:got (read-sexps "#x10 #b101 #e1.0 1/3 1e10")))

    (test "an unterminated form is reported at its opener"
      (assert-equal #:expect '((1 1 unterminated))
        #:got (read-errors "(a (b c)"))
      (assert-equal #:expect '((a (b c)))
        #:got (read-sexps "(a (b c)")))

    (test "an unmatched closer is reported, and the rest of the file is read"
      (assert-equal #:expect '((1 2 unmatched))
        #:got (read-errors "a) b"))
      (assert-equal #:expect '(a ")" b)
        #:got (read-sexps "a) b")))

    (test "an unterminated string is reported at its quote"
      (assert-equal #:expect '((1 4 unterminated) (1 1 unterminated))
        #:got (read-errors "(a \"abc")))

    (test "CRLF is read as one newline per line"
      (assert-same-text #:expect "(a\n b)\n"
        #:got (round-trip "(a\r\n b)\r\n")))

    (test "locations count characters, not bytes"
      (call-with-values (lambda () (read-source-string "(ä b)"))
        (lambda (ast errors)
          (let ((second (cadr (node-elements (car ast)))))
            (assert-equal #:expect 1
              #:got (source-location-line (node-start second)))
            (assert-equal #:expect 4
              #:got (source-location-column (node-start second)))
            (assert-equal #:expect 4
              #:got (source-location-offset (node-start second)))))))

    (test "an inline comment knows it follows code"
      (assert-equal #:expect '(#f #t)
        #:got (inline-flags ";; top\n(a ; inline\n b)")))

    (test "comments are kept, every one of them"
      (assert-equal #:expect '(";; one" ";; two" "; not three" ";; three")
        #:got (comment-texts ";; one\n;; two\n(a) ; not three\n;; three")))

    (test "blank lines are nodes, so they survive a round trip"
      (assert-same-text #:expect "(a)\n\n\n(b)\n"
        #:got (round-trip "(a)\n\n\n(b)\n")))

    (test "a whole file of trivia round-trips byte for byte"
      (let ((source ";; a comment\n\n;; another\n"))
        (assert-same-text #:expect source #:got (round-trip source))))

    (test "a file that is already formatted round-trips byte for byte"
      (let ((source "(define (square x)\n  (* x x))\n\n;; done\n"))
        (assert-same-text #:expect source #:got (round-trip source))))))
