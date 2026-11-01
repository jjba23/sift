;; sift --- what a file declares is not a call
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Two rules are about calling: `keyword-arguments` says a call has more
;; positional arguments than a reader can hold, and `too-many-arguments` says a
;; parameter list has.  A declaration looks enough like a call to fool both --
;; `(use-modules (a) (b) (c) (d) (e) (f))' names six libraries in one form, and
;; `(defun f (a b c d e f g) ...)' keeps its parameters in a list of their own --
;; so this suite is the corpus that keeps them apart: every way a file of each
;; dialect says what it uses, what it exports and what it defines, and the
;; assertion that neither rule says anything about any of it.
;;
;; The other half matters as much: a rule that has been taught to ignore
;; everything is a rule that says nothing at all.  The last four tests are calls
;; of the same shape as the declarations above, and they are reported.

(define-module (tests declarations)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift lint)
  #:export (spec))

;;; ===== The corpus =====

(define scheme-declarations
  '("(define-module (demo thing)\n  #:declarative? #t\n  #:use-module (srfi srfi-1)\n  #:use-module (srfi srfi-13)\n  #:autoload (ice-9 format) (format)\n  #:export (one two three four five six seven)\n  #:re-export (car cdr)\n  #:duplicates (merge-generics replace warn-override-core warn last))\n"
    "(use-modules (srfi srfi-1)\n             (srfi srfi-13)\n             (ice-9 format)\n             (ice-9 match)\n             (sift records)\n             (sift reader))\n"
    "(import (scheme base)\n        (scheme write)\n        (only (srfi 1) fold)\n        (prefix (rnrs) r6:)\n        (rename (srfi 2) (and-let* let-and))\n        (except (guile) call-with-current-continuation))\n"
    "(export one\n        two\n        three\n        four\n        five\n        six\n        (rename seven seventh))\n"
    "(define-library (demo lib)\n  (export a b c d e f g)\n  (import (scheme base) (scheme write))\n  (include \"body.scm\"))\n"
    "(library (demo r6rs)\n  (export a b c d e f g)\n  (import (rnrs)))\n"
    "(declare (pure? #t) (blocked #:all) (names one two three four five six))\n"))

(define elisp-declarations
  '("(require 'cl-lib)\n(require 'subr-x)\n(provide 'demo)\n"
    "(use-package flymake\n  :ensure t\n  :hook (prog-mode . flymake-mode)\n  :config (setq flymake-no-changes-timeout 0.5))\n"
    "(declare-function sift-mode \"sift\" (&optional arg))\n"
    "(eval-when-compile (require 'cl-lib))\n"
    "(setq one 1 two 2 three 3 four 4 five 5 six 6)\n"))

(define cl-declarations
  '("(defpackage :demo\n  (:use :cl :alexandria)\n  (:export #:run #:one #:two #:three #:four #:five #:six)\n  (:import-from :serapeum #:assocdr #:dict)\n  (:shadow #:car)\n  (:nicknames :demo-alt))\n"
    "(in-package :demo)\n"
    "(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))\n"
    "(proclaim '(ftype (function (fixnum fixnum) fixnum) add))\n"
    "(defmethod area ((shape circle) &key (scale 1) &allow-other-keys)\n  scale)\n"
    "(defun add (a b &optional (c 1) &key (d 2) &allow-other-keys)\n  (+ a b c d))\n"))

;; Forms that *have* parameter lists.  A parameter list is not a call, so
;; `keyword-arguments` has nothing to say about one however long it is -- and
;; `too-many-arguments` has something to say, which is why these are asserted
;; apart from the declarations above.
(define parameter-list-corpus
  '((elisp . "(defun demo-f (a b c d e f g)\n  \"Return them.\"\n  (list a b c d e f g))\n")
    (elisp . "(lambda (a b c d e f g) (list a b c d e f g))\n")
    (scheme . "(define (demo-f a b c d e f g)\n  (list a b c d e f g))\n")
    (scheme . "(define* (demo-g a b #:key c d e f g h)\n  (list a b))\n")
    (cl . "(defmethod area ((shape circle) &key (scale 1) &allow-other-keys)\n  scale)\n")))

(define (declaration-corpus)
  (append (map (lambda (source) (cons 'scheme source)) scheme-declarations)
          (map (lambda (source) (cons 'elisp source)) elisp-declarations)
          (map (lambda (source) (cons 'cl source)) cl-declarations)))

(define (findings-of source dialect checkers)
  "Return the findings @var{checkers} report about @var{source}."
  (map (lambda (warning)
         (format #f "~a ~a"
                 (lint-warning-checker warning)
                 (lint-warning-message warning)))
       (lint-source source #:dialect dialect #:checkers checkers)))

;;; ===== The suite =====

(define (spec)
  (suite "declarations: a declaration is not a call"

    (test "no rule about calls reports a declaration"
      (for-each
       (lambda (entry)
         (let ((dialect (car entry))
               (source (cdr entry)))
           (assert-equal
             #:expect '()
             #:got (findings-of source dialect '(keyword-arguments too-many-arguments))
             #:name (format #f "~a declaration: ~a" dialect
                            (car (string-split source #\newline))))))
       (declaration-corpus)))

    (test "a call with too many positional arguments is still reported"
      (assert-equal #:expect 1
        #:got (length (findings-of "(make-window a b c d e f g)\n"
                                   'scheme '(keyword-arguments)))))

    (test "a parameter list with too many required parameters is still reported"
      (assert-equal #:expect 1
        #:got (length (findings-of "(define (place x y width height colour outline)\n  (list x))\n"
                                   'scheme '(too-many-arguments)))))

    (test "a parameter list of keywords and optionals is not a call"
      (assert-equal #:expect '()
        #:got (findings-of "(define* (window a b #:key c d e f g h)\n  (list a b))\n"
                           'scheme '(keyword-arguments too-many-arguments))))

    (test "a parameter list is not a call, however long it is"
      (for-each
       (lambda (entry)
         (assert-equal
           #:expect '()
           #:got (findings-of (cdr entry) (car entry) '(keyword-arguments))
           #:name (format #f "~a parameter list: ~a" (car entry)
                          (car (string-split (cdr entry) #\newline)))))
       parameter-list-corpus))

    (test "a definition's body is still read as code"
      ;; the point of the exclusion is the parameter list, not the body: a call
      ;; inside a definition is a call
      (assert-equal #:expect 1
        #:got (length (findings-of "(defun demo-f (a)\n  \"Docstring here.\"\n  (make-window a b c d e f g))\n"
                                   'elisp '(keyword-arguments)))))))
