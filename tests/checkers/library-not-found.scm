;; sift -- checker: library-not-found
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; "Does this library exist" is a question about the project, not about the
;; file, so this is the one rule that reads its neighbours: the libraries a
;; project declares, found by the `(library ...)' or `(define-library ...)'
;; form each of them writes down.  The tests run against a real directory --
;; the rule has no other way to know anything -- and they state the two
;; silences that keep it honest: a library sift knows by name (a standard one,
;; an SRFI), and a project that declares no library at all, where there is
;; nothing to compare an import against.

(define-module (tests checkers library-not-found)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (findings path)
  "Return the @code{(line column)} of every finding about @var{path}."
  (map (lambda (warning)
         (let ((location (lint-warning-location warning)))
           (list (source-location-line location)
                 (source-location-column location))))
       (lint-file path #:severity 'info
                  #:checkers '(library-not-found unused-import))))

(define (write-project directory)
  "Write a project of one library and two programs into @var{directory}."
  (spit (string-append directory "/lib/my.scm")
        "(define-library (my lib)\n  (export run stop)\n  (import (scheme base))\n  (define (run) 1)\n  (define (stop) 2))\n")
  (spit (string-append directory "/uses.scm")
        "(import (scheme base) (my lib))\n(display (run))\n")
  (spit (string-append directory "/missing.scm")
        "(import (scheme base) (other lib))\n(display 1)\n")
  (spit (string-append directory "/ignores.scm")
        "(import (scheme base) (my lib))\n(display 1)\n"))

(define (spec)
  (suite "checker: library-not-found"

    (test "a library the project declares is found"
      (with-temp-directory
       (lambda (directory)
         (write-project directory)
         (assert-equal #:expect '()
           #:got (findings (string-append directory "/uses.scm"))))))

    (test "an import of a library no file declares is reported"
      (with-temp-directory
       (lambda (directory)
         (write-project directory)
         (assert-equal #:expect '((1 23))
           #:got (findings (string-append directory "/missing.scm"))))))

    (test "a library the file declares itself is found"
      (with-temp-directory
       (lambda (directory)
         (write-project directory)
         (let ((path (string-append directory "/self.scm")))
           (spit path
                 "(define-library (my lib)\n  (export run)\n  (import (scheme base) (my lib))\n  (define (run) 1))\n")
           (assert-equal #:expect '() #:got (findings path))))))

    (test "a project that declares no library is not judged"
      (with-temp-directory
       (lambda (directory)
         (let ((path (string-append directory "/plain.scm")))
           (spit path "(import (somewhere else))\n(display 1)\n")
           (assert-equal #:expect '() #:got (findings path))))))

    (test "a standard library is never reported missing"
      (with-temp-directory
       (lambda (directory)
         (write-project directory)
         (let ((path (string-append directory "/stdlib.scm")))
           (spit path "(import (scheme base) (srfi 1) (rnrs) (ice-9 match))\n(display 1)\n")
           (assert-equal #:expect '() #:got (findings path))))))

    (test "a library imported where nothing it exports is used is reported"
      (with-temp-directory
       (lambda (directory)
         (write-project directory)
         (assert-equal #:expect '((1 23))
           #:got (findings (string-append directory "/ignores.scm"))))))))
