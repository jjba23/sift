;; sift -- checker: special-variable-name
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Common Lisp marks a dynamically bound variable with earmuffs, `*port*', so
;; that a reader can see a special variable without looking it up; a constant
;; is `+pi+'.  Emacs Lisp has no such convention, and a `*name*' there is a
;; sign that the author came from the other dialect.

(define-module (tests checkers special-variable-name)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: special-variable-name"

    (test "reports a Common Lisp special variable without earmuffs"
      (assert-warnings #:source "(defparameter port 8080)\n" #:dialect 'cl
        #:checker 'special-variable-name #:expect '((1 15))))

    (test "accepts an earmuffed one"
      (assert-warnings #:source "(defparameter *port* 8080)\n" #:dialect 'cl
        #:checker 'special-variable-name #:expect '()))

    (test "reports a Common Lisp constant without plus signs"
      (assert-warnings #:source "(defconstant pi 3.14)\n" #:dialect 'cl
        #:checker 'special-variable-name #:expect '((1 14))))

    (test "accepts a constant written with plus signs"
      (assert-warnings #:source "(defconstant +pi+ 3.14)\n" #:dialect 'cl
        #:checker 'special-variable-name #:expect '()))

    (test "reports earmuffs in Emacs Lisp"
      (assert-warnings #:source "(defvar *port* 8080)\n" #:dialect 'elisp
        #:checker 'special-variable-name #:expect '((1 9))))

    (test "accepts a plain Emacs Lisp variable"
      (assert-warnings #:source "(defvar port 8080)\n" #:dialect 'elisp
        #:checker 'special-variable-name #:expect '()))))
