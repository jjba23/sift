;; sift --- which files are Lisp files
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The extension list is the answer for a project that has never thought about
;; the question, and the filter is the answer for one that has.  Both halves
;; matter: `.sps', `.sld' and `.sls' are Scheme and have to be read like the
;; `.scm' files beside them, and a pattern is how a project says "not that
;; corner".
;;
;; The glob rules are stated here rather than assumed: `*' stops at a
;; separator, `**' crosses one, a pattern with no separator is matched against
;; the file's own name, and the last matching pattern decides -- which is what
;; makes `!vendor/**' able to undo the pattern before it.

(define-module (tests files)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:use-module (sift files)
  #:use-module (sift config)
  #:use-module (tests helpers)
  #:export (spec))

(define* (filter-of yaml)
  "Return the file-filter a configuration file sets, read from @var{yaml}."
  (with-temp-directory
   (lambda (directory)
     (let ((path (string-append directory "/sift.yaml")))
       (spit path yaml)
       (call-with-values
         (lambda () (load-config #:cli '() #:config-file path #:trust #t))
         (lambda (config layers) (config-ref config 'file-filter)))))))

(define (spec)
  (suite "files: extensions and filters"

    ;; ===== extensions =====

    (test "the Scheme extensions sift reads include the library and program ones"
      (map (lambda (extension)
             (assert-true (lisp-path? (string-append "file" extension))
               #:name (format #f "~a is a Lisp file" extension)))
           '(".scm" ".ss" ".sld" ".sls" ".sps" ".el" ".lisp" ".cl" ".asd")))

    (test "the Racket extensions are sift's files too"
      ;; a Racket project is four extensions: a module, a data module, a
      ;; library the test runner loads, and a Scribble document
      (map (lambda (extension)
             (assert-true (lisp-path? (string-append "file" extension))
               #:name (format #f "~a is a Lisp file" extension)))
           '(".rkt" ".rktd" ".rktl")))

    (test "a file sift has no extension for is not one of its files"
      (assert-equal #:expect #f #:got (lisp-path? "notes.txt")))

    (test "a compressed source is read as the text it holds"
      ;; Emacs ships its own library as `.el.gz', and a file the reader cannot
      ;; name is a file navigation never opens.  The command line keeps the
      ;; uncompressed extension, so a write never lands on a `.gz'.
      (assert-equal #:expect "foo.el" #:got (without-compression "foo.el.gz"))
      (assert-equal #:expect "foo.el" #:got (without-compression "foo.el"))
      (assert-equal #:expect "(defun widget-size (size)\n  (* size 2))\n"
        #:got (read-file-text (fixture "compressed" "widget.el.gz"))))

    ;; ===== globs =====

    (test "* stops at a separator"
      (assert-equal #:expect #f
        #:got (glob-match? "src/*.scm" "src/deeper/a.scm"))
      (assert-equal #:expect #t
        #:got (glob-match? "src/*.scm" "src/a.scm")))

    (test "** crosses a separator"
      (assert-equal #:expect #t
        #:got (glob-match? "src/**/*.scm" "src/a/b/c.scm")))

    (test "a pattern with no separator is matched against the file's own name"
      (assert-equal #:expect #t #:got (glob-match? "*.sps" "deep/down/a.sps")))

    (test "a dot in a pattern is a dot, not any character"
      (assert-equal #:expect #f #:got (glob-match? "*.scm" "a.scmx")))

    ;; ===== the filter =====

    (test "no filter falls back to the extensions"
      (assert-equal #:expect #t #:got (file-included? #f "src/a.scm"))
      (assert-equal #:expect #f #:got (file-included? '() "src/a.txt")))

    (test "a compressed file is navigation's, not the command line's"
      ;; `sift format --write' must never write decompressed text over a `.gz'
      (assert-equal #:expect #f #:got (file-included? #f "src/foo.el.gz")))

    (test "a filter replaces the extensions"
      (assert-equal #:expect #t #:got (file-included? '("*.txt") "src/a.txt"))
      (assert-equal #:expect #f #:got (file-included? '("*.txt") "src/a.scm")))

    (test "a negated pattern takes a file back out"
      (assert-equal #:expect #f
        #:got (file-included? '("src/**" "!src/gen/**") "src/gen/a.scm"))
      (assert-equal #:expect #t
        #:got (file-included? '("src/**" "!src/gen/**") "src/a.scm")))

    (test "the last pattern that matches decides"
      (assert-equal #:expect #t
        #:got (file-included? '("!src/**" "src/keep/**") "src/keep/a.scm")))

    (test "a pattern is read relative to the root of the walk"
      (assert-equal #:expect #t
        #:got (file-included? '("src/**") "/tmp/project/src/a.scm"
                              "/tmp/project")))

    ;; ===== the setting =====

    (test "a single pattern is a filter with one rule"
      (assert-equal #:expect '("*.scm")
        #:got (filter-of "file-filter: \"*.scm\"\n")))

    (test "a list of patterns is read as it is written"
      (assert-equal #:expect '("src/**" "!gen/**")
        #:got (filter-of "file-filter:\n  - \"src/**\"\n  - \"!gen/**\"\n")))

    (test "no filter in the file is no filter at all"
      (assert-equal #:expect #f #:got (filter-of "line-length: 90\n")))))