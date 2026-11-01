;; sift --- formatter tests, Emacs Lisp
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Emacs Lisp indents by two from the opening parenthesis for the forms the
;; Emacs Lisp Reference Manual documents, and a docstring is a value like any
;; other, so it is never reflowed.  `use-package' is asserted next to them
;; because it is the one third-party macro common enough to be in the table,
;; and because it says how it is indented itself -- `(declare (indent defun))'
;; -- so a formatter that disagrees with Emacs on it is simply wrong.  All of
;; that sits here, next to the three passes over the corpus.

(define-module (tests format-elisp)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:use-module (sift format)
  #:use-module (tests helpers)
  #:export (spec))

(define (elisp-pairs)
  (filter (lambda (pair) (eq? (dialect-of (car pair)) 'elisp))
          (fixture-pairs)))

(define (spec)
  (suite "format: Emacs Lisp"

    (test "every fixture formats to its expected file"
      (map assert-fixture (elisp-pairs)))

    (test "every expected file is a fixed point of formatting"
      (map assert-fixture-idempotent (elisp-pairs)))

    (test "a defun indents its docstring and body two columns in"
      (assert-same-text
       #:expect "(defun f (x)\n  \"Return X.\"\n  (when x\n    (message \"x\")))"
       #:got (format-string "(defun f (x)\n\"Return X.\"\n(when x\n(message \"x\")))"
                            #:dialect 'elisp)))

    (test "a let binds in a data list, lined up under the first binding"
      (assert-same-text
       #:expect "(let ((a 1)\n      (b 2))\n  (+ a b))"
       #:got (format-string "(let ((a 1)\n(b 2))\n(+ a b))" #:dialect 'elisp)))

    (test "condition-case indents its protected form and handlers two in"
      (assert-same-text
       #:expect "(condition-case nil\n  (foo)\n  (error (message \"no\")))"
       #:got (format-string "(condition-case nil\n(foo)\n(error (message \"no\")))"
                            #:dialect 'elisp)))

    (test "documented forms the table knows are indented from the opener"
      (assert-same-text
       #:expect "(with-current-buffer buf\n  (goto-char (point-min)))"
       #:got (format-string "(with-current-buffer buf\n(goto-char (point-min)))"
                            #:dialect 'elisp)))

    ;; `use-package' says `(declare (indent defun))', so Emacs puts its
    ;; keywords, and the bodies under them, two columns in from the opener.
    ;; sift has to agree, because the point of the table is that the formatter
    ;; and the editor do not disagree about a form as common as this one.
    (test "a use-package form puts its keywords and their bodies two columns in"
      (assert-same-text
       #:expect "(use-package evil\n  :ensure t\n  :init\n  (setq evil-want-integration t)\n  :config\n  (evil-mode 1))"
       #:got (format-string "(use-package evil\n:ensure t\n:init\n(setq evil-want-integration t)\n:config\n(evil-mode 1))"
                            #:dialect 'elisp)))

    ;; The generic rule -- arguments line up under the first one -- would line
    ;; the keywords up under the package name, which is what a call does and
    ;; what a `use-package' form does not.
    (test "use-package keywords do not line up under the package name"
      (assert-same-text
       #:expect "(use-package magit\n  :ensure t\n  :bind\n  (\"C-x g\" . magit-status))"
       #:got (format-string "(use-package magit\n             :ensure t\n             :bind\n             (\"C-x g\" . magit-status))"
                            #:dialect 'elisp)))

    (test "strings, escapes and characters are preserved"
      (assert-same-text
       #:expect "(message \"a  b\\n\" ?\\C-x)"
       #:got (format-string "(message   \"a  b\\n\"   ?\\C-x)" #:dialect 'elisp)))

    ;; A square bracket is not Emacs Lisp syntax.  `brackets: square' in a
    ;; project's sift.yaml reads as a choice about style, so it has to mean
    ;; nothing here rather than writing a file Emacs cannot read.
    (test "the square bracket style does not reach Emacs Lisp"
      (assert-same-text
       #:expect "(cond ((null x) 1)\n      (t 2))"
       #:got (format-string "(cond ((null x) 1)\n(t 2))"
                            #:dialect 'elisp #:brackets 'square)))

    (test "formatting Emacs Lisp is idempotent"
      (assert-idempotent #:source "(defun f (x)\n  \"Doc.\"\n  (list x))\n"
                         #:dialect 'elisp))))
