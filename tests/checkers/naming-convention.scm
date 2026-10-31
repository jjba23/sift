;; sift -- checker: naming-convention
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; A Lisp name is a phrase with hyphens in it: `read-file', not `read_file'
;; and not `readFile'.  The rule looks at the name of a definition, which is
;; where the convention is decided, and nowhere else -- a variable bound to
;; somebody else's function cannot be renamed by a linter.

(define-module (tests checkers naming-convention)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: naming-convention"

    (test "reports an underscore in a procedure name"
      (assert-warnings #:source "(define (read_file path) path)\n"
        #:checker 'naming-convention #:expect '((1 10))))

    (test "reports camelCase in a procedure name"
      (assert-warnings #:source "(define (readFile path) path)\n"
        #:checker 'naming-convention #:expect '((1 10))))

    (test "reports a capitalised name"
      (assert-warnings #:source "(define (Read path) path)\n"
        #:checker 'naming-convention #:expect '((1 10))))

    (test "reports a variable definition too"
      (assert-warnings #:source "(define max_size 10)\n"
        #:checker 'naming-convention #:expect '((1 9))))

    (test "reports an Emacs Lisp defun"
      (assert-warnings #:source "(defun my_func () 1)\n" #:dialect 'elisp
        #:checker 'naming-convention #:expect '((1 8))))

    (test "reports a Common Lisp defparameter"
      (assert-warnings #:source "(defparameter httpPort 80)\n" #:dialect 'cl
        #:checker 'naming-convention #:expect '((1 15))))

    (test "accepts the usual hyphenated name"
      (assert-warnings #:source "(define (read-file path) path)\n"
        #:checker 'naming-convention #:expect '()))

    (test "accepts a name that is punctuation"
      (assert-warnings #:source "(define (->string x) x)\n"
        #:checker 'naming-convention #:expect '()))

    (test "says nothing about a symbol that is not a definition"
      (assert-warnings #:source "(display (list 'max_size 1))\n"
        #:checker 'naming-convention #:expect '()))))
