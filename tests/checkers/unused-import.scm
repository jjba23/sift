;; sift -- checker: unused-import
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; An import nothing uses is a claim the file stopped keeping: the reader looks
;; up the library, finds the name, and the name is never mentioned again.  What
;; sift can know is bounded on purpose -- the names an `only' clause writes
;; down, the pairs a `rename' writes down, and a library whose own `(export
;; ...)' it can read -- so the tests state both halves: the import that is
;; reported, and the plain `(import (scheme base))' that is not.

(define-module (tests checkers unused-import)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: unused-import"

    (test "reports the name of an `only` clause nothing uses"
      (assert-warnings
       #:source "(import (only (srfi 1) fold car))\n(display (fold + 0 (list 1 2)))\n"
       #:checker 'unused-import #:expect '((1 29))))

    (test "the fix deletes the name and the space before it"
      (assert-fix
       #:source "(import (only (srfi 1) fold car))\n(display (fold + 0 (list 1 2)))\n"
       #:checker 'unused-import
       #:expected "(import (only (srfi 1) fold))\n(display (fold + 0 (list 1 2)))\n"))

    (test "an `only` clause with nothing used is one finding, on the form"
      (assert-warnings
       #:source "(import (only (srfi 1) car cdr))\n(display 1)\n"
       #:checker 'unused-import #:expect '((1 1))))

    (test "the fix removes the whole import when it is the only thing on its line"
      (assert-fix
       #:source "(import (only (srfi 1) car cdr))\n(display 1)\n"
       #:checker 'unused-import
       #:expected "(display 1)\n"))

    (test "one unused spec of two is deleted, and the form stays"
      (assert-fix
       #:source "(import (scheme base) (only (srfi 1) car))\n(display 1)\n"
       #:checker 'unused-import
       #:expected "(import (scheme base))\n(display 1)\n"))

    (test "reports a renamed import nothing uses, by its new name"
      (assert-fix
       #:source "(import (rename (srfi 1) (fold foldl) (car first)))\n(display (foldl + 0 (list 1)))\n"
       #:checker 'unused-import
       #:expected "(import (rename (srfi 1) (fold foldl)))\n(display (foldl + 0 (list 1)))\n"))

    (test "a name used only inside a quote does not count as used"
      (assert-warnings
       #:source "(import (only (srfi 1) car))\n(display '(car))\n"
       #:checker 'unused-import #:expect '((1 1))))

    (test "a plain import of a library sift cannot read is left alone"
      (assert-warnings
       #:source "(import (scheme base) (srfi 1))\n(display 1)\n"
       #:checker 'unused-import #:expect '()))

    (test "an import used inside a library body is read"
      (assert-warnings
       #:source "(define-library (my lib)\n  (export run)\n  (import (scheme base) (only (srfi 1) car))\n  (define (run) 1))\n"
       #:checker 'unused-import #:expect '((3 25))))

    ;; Guile writes its imports as clauses of `define-module', and a clause is
    ;; an import site like an `(import ...)' form is: the same question, the
    ;; same answer, and a fix that takes the keyword with the spec -- a fix that
    ;; left `#:use-module' behind would leave a declaration of nothing.
    (test "a `#:use-module` clause nothing uses is reported, keyword and all"
      (with-temp-directory
       (lambda (directory)
         (spit (string-append directory "/records.scm")
               "(define-module (proj records)\n  #:export (make-warning))\n(define (make-warning a) a)\n")
         (let* ((path (string-append directory "/client.scm"))
                (source
                 (string-append
                  "(define-module (proj client)\n"
                  "  #:use-module (proj records)\n"
                  "  #:export (f))\n"
                  "(define (f x) x)\n")))
           (spit path source)
           (let ((warnings (lint-file path #:severity 'info
                                      #:checkers '(unused-import))))
             (assert-equal #:expect 1 #:got (length warnings)
               #:name "one finding, on the clause")
             (assert-same-text
              #:expect (string-append "(define-module (proj client)\n"
                                      "  #:export (f))\n"
                                      "(define (f x) x)\n")
              #:got (apply-fixes source warnings)
              #:name "the fix removes the clause and the line it was on"))))))

    (test "a `#:use-module` clause whose names are used is silent"
      (with-temp-directory
       (lambda (directory)
         (spit (string-append directory "/records.scm")
               "(define-module (proj records)\n  #:export (make-warning))\n(define (make-warning a) a)\n")
         (let ((path (string-append directory "/client.scm")))
           (spit path
                 (string-append
                  "(define-module (proj client)\n"
                  "  #:use-module (proj records)\n"
                  "  #:export (f))\n"
                  "(define (f x) (make-warning x))\n"))
           (assert-equal #:expect 0
             #:got (length (lint-file path #:severity 'info
                                      #:checkers '(unused-import)))
             #:name "the name the clause brings in is used")))))

    (test "every name in an `only` clause used is silent"
      (assert-warnings
       #:source "(import (only (srfi 1) fold))\n(display (fold + 0 (list 1)))\n"
       #:checker 'unused-import #:expect '()))))
