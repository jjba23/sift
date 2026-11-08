;; sift --- formatter tests, Common Lisp
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Common Lisp shares the two-column body rule with the other dialects and
;; adds a few forms of its own: `defpackage' clauses, `defclass' slots,
;; `handler-case' clauses and the LOOP keywords, which are ordinary atoms and
;; therefore line up like any other argument.

(define-module (tests format-cl)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:use-module (sift format)
  #:use-module (tests helpers)
  #:export (spec))

(define (cl-pairs)
  (filter (lambda (pair) (eq? (dialect-of (car pair)) 'cl))
          (fixture-pairs)))

(define (spec)
  (suite "format: Common Lisp"

    (test "every fixture formats to its expected file"
      (map assert-fixture (cl-pairs)))

    (test "every expected file is a fixed point of formatting"
      (map assert-fixture-idempotent (cl-pairs)))

    (test "a defpackage indents its clauses two columns in"
      (assert-same-text
       #:expect "(defpackage :demo\n  (:use :cl)\n  (:export #:run))"
       #:got (format-string "(defpackage :demo\n(:use :cl)\n(:export #:run))"
                            #:dialect 'cl)))

    (test "a defclass indents its slots two columns in"
      (assert-same-text
       #:expect "(defclass point ()\n  ((x :initarg :x)\n   (y :initarg :y)))"
       #:got (format-string "(defclass point ()\n((x :initarg :x)\n(y :initarg :y)))"
                            #:dialect 'cl)))

    (test "handler-case clauses align under each other"
      (assert-same-text
       #:expect "(handler-case (foo)\n  (error (e)\n    (print e)))"
       #:got (format-string "(handler-case (foo)\n(error (e)\n(print e)))"
                            #:dialect 'cl)))

    ;; A pair form states its places and values as ordinary arguments, and
    ;; Emacs lines the second pair up under the first place, in Common Lisp as
    ;; it does in Emacs Lisp.
    (test "a setq lines its pairs up under the first variable"
      (assert-same-text
       #:expect "(setq a 1\n      b 2)"
       #:got (format-string "(setq a 1\nb 2)" #:dialect 'cl)))

    (test "a setf lines its places up under the first one"
      (assert-same-text
       #:expect "(setf a 1\n      b 2)"
       #:got (format-string "(setf a 1\nb 2)" #:dialect 'cl)))

    (test "a LOOP keeps its keyword atoms and indents them two in"
      (assert-same-text
       #:expect "(loop :for x :in xs\n  :do (print x))"
       #:got (format-string "(loop :for x :in xs\n:do (print x))" #:dialect 'cl)))

    (test "feature conditionals keep both halves on one line"
      (assert-same-text
       #:expect "#+sbcl (defun f ())\n#-sbcl (defun f ())"
       #:got (format-string "#+sbcl   (defun f ())\n#-sbcl   (defun f ())"
                            #:dialect 'cl)))

    (test "line endings are normalised to LF"
      (assert-same-text #:expect "(a b)\n"
                        #:got (format-string "(a b)\r\n" #:dialect 'cl)))

    (test "a read macro keeps the datum it applies to glued to it"
      (assert-same-text #:expect "(defparameter *a* #2A((1 2)))\n"
                        #:got (format-string "(defparameter   *a*   #2A((1 2)))\n"
                                             #:dialect 'cl))
      (assert-same-text #:expect "(defparameter *c* #c(1 2))\n"
                        #:got (format-string "(defparameter *c* #c(1 2))\n"
                                             #:dialect 'cl))
      (assert-same-text #:expect "(probe-file #P\"/tmp/x\")\n"
                        #:got (format-string "(probe-file #P\"/tmp/x\")\n"
                                             #:dialect 'cl)))

    (test "formatting Common Lisp is idempotent"
      (assert-idempotent #:source "(defun f (x)\n  \"Doc.\"\n  (list x))\n"
                         #:dialect 'cl))))
