;; sift --- reader tests, Emacs Lisp
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Emacs Lisp has its own character-literal syntax, its own function quote,
;; and no block comments at all.  These tests pin those differences down --
;; including the two places where sift is deliberately permissive: a `#|'
;; comment and a `#;' datum comment in an Emacs Lisp file are reported, but
;; the text is still kept, because a formatter's first duty is not to lose
;; anything.

(define-module (tests reader-elisp)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (tests helpers)
  #:export (spec))

(define (read-elisp source)
  (read-sexps source #:dialect 'elisp))

(define (read-elisp-ast source)
  "Return the data nodes of @var{source}, read as Emacs Lisp."
  (call-with-values (lambda () (read-source-string source #:dialect 'elisp))
    (lambda (ast errors) (filter node-data? ast))))

(define (errors-elisp source)
  (read-errors source #:dialect 'elisp))

(define (round-trip source)
  (call-with-values (lambda () (read-source-string source #:dialect 'elisp))
    (lambda (ast errors) (ast->text ast))))

(define (spec)
  (suite "reader: Emacs Lisp"

    (test "character literals keep their spelling"
      (assert-equal #:expect '("?a" "?\\n" "?\\C-x" "?\\M-x" "?\\s" "?\\(")
        #:got (read-elisp "?a ?\\n ?\\C-x ?\\M-x ?\\s ?\\(")))

    (test "a character literal with a meta and a control modifier is one atom"
      (assert-equal #:expect '("?\\M-\\C-x")
        #:got (read-elisp "?\\M-\\C-x")))

    (test "a function quote is a prefix, like a quote"
      (assert-equal #:expect '((function foo) (quote bar))
        #:got (read-elisp "#'foo 'bar")))

    (test "a defun keeps its name, argument list, docstring and body"
      ;; a string atom keeps its quotes: the reader never decodes an escape,
      ;; so what comes back is the text the author typed
      (assert-equal #:expect '((defun add (a b)
                                      "\"Add A and B.\""
                                      (+ a b)))
        #:got (read-elisp "(defun add (a b)\n  \"Add A and B.\"\n  (+ a b))")))

    (test "vectors are read as vectors"
      (assert-equal #:expect '(#(1 2 3))
        #:got (read-elisp "[1 2 3]")))

    (test "a hash-table literal is a read macro over a list"
      ;; Emacs prints a hash table as `#s(hash-table ...)', and the space that
      ;; a formatter would put after the `#' would stop it being one.
      (assert-same-text #:expect "(defvar t #s(hash-table test equal data (\"a\" 1)))"
                        #:got (round-trip "(defvar t #s(hash-table test equal data (\"a\" 1)))")))

    (test "a read-circle label is a prefix over its datum"
      (assert-same-text #:expect "#1=(a . #1#)"
                        #:got (round-trip "#1=(a . #1#)")))

    (test "a Scheme character literal is reported, and still kept"
      (assert-equal #:expect '((1 1 character))
        #:got (errors-elisp "#\\a"))
      (assert-same-text #:expect "#\\a" #:got (round-trip "#\\a")))

    (test "a backslash quotes the next character of a symbol"
      ;; `\(' is a symbol named `(': without the escape the reader would open
      ;; a list that never closes, which is how a real Emacs Lisp file broke.
      (assert-equal #:expect '() #:got (errors-elisp "\\( \\) a\\#b"))
      (assert-equal #:expect '("\\(" "\\)" "a\\#b")
        #:got (map node-value
                   (filter node-atom? (read-elisp-ast "\\( \\) a\\#b")))))

    (test "a bar is an ordinary symbol character, not an escape"
      (assert-equal #:expect '() #:got (errors-elisp "(| a b)"))
      (assert-equal #:expect '("|" "a" "b")
        #:got (map node-value
                   (node-elements (car (read-elisp-ast "(| a b)"))))))

    (test "a brace is a symbol character, not a bracket"
      (assert-equal #:expect '("{" "_" "}")
        #:got (map node-value (filter node-atom? (read-elisp-ast "{ _ }")))))

    (test "a dotted pair keeps its dot"
      (assert-same-text #:expect "(a . b)" #:got (round-trip "(a . b)")))

    (test "a block comment is reported, and still kept"
      (assert-equal #:expect '((1 1 block))
        #:got (errors-elisp "#| not elisp |# x"))
      (assert-same-text #:expect "#| not elisp |# x"
                        #:got (round-trip "#| not elisp |# x")))

    (test "a datum comment is reported, and still kept"
      (assert-equal #:expect '((1 1 the))
        #:got (errors-elisp "#;foo bar"))
      (assert-same-text #:expect "#;foo bar"
                        #:got (round-trip "#;foo bar")))

    (test "the lexical-binding header is an ordinary comment"
      (assert-same-text #:expect ";;; foo.el --- a package -*- lexical-binding: t; -*-\n"
                        #:got (round-trip ";;; foo.el --- a package -*- lexical-binding: t; -*-\n")))

    (test "a shebang line is kept"
      (assert-same-text #:expect "#!/usr/bin/env emacs --script\n(message \"hi\")\n"
                        #:got (round-trip "#!/usr/bin/env emacs --script\n(message \"hi\")\n")))

    (test "docstrings with escapes are not respelled"
      (assert-same-text #:expect "(defun f ()\n  \"Say \\\"hi\\\".\"\n  nil)"
                        #:got (round-trip "(defun f ()\n  \"Say \\\"hi\\\".\"\n  nil)")))

    (test "the dialect is detected from the extension"
      (assert-equal #:expect 'elisp
        #:got (detect-dialect #:file "foo.el")))

    (test "a compressed extension is read through its compression"
      ;; the Emacs library is installed as `.el.gz', and a file whose dialect
      ;; cannot be named is a file a jump never reaches
      (assert-equal #:expect 'elisp
        #:got (detect-dialect #:file "foo.el.gz")))

    (test "the dialect is detected from a lexical-binding marker"
      (assert-equal #:expect 'elisp
        #:got (detect-dialect #:source ";;; -*- lexical-binding: t; -*-\n")))

    (test "an Emacs Lisp file round-trips byte for byte"
      (let ((source ";;; demo.el --- a demo -*- lexical-binding: t; -*-\n\n(defun demo (x)\n  \"Return X.\"\n  (message \"%s\" x))\n\n(provide 'demo)\n"))
        (assert-same-text #:expect source #:got (round-trip source))))))
