;; sift --- which files are Lisp files
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A tool that walks a directory has to answer one question before it reads
;; anything: which of these files are mine?  The answer is a list of
;; extensions, and every project eventually has a file that is not on it --
;; an R6RS program in `.sps', a file generated with a different suffix -- or a
;; directory it does not want walked at all.
;;
;; So the answer is a *filter*: patterns a file must match, with `*' stopping
;; at a directory and `**' crossing one, and a leading `!' for the files and
;; directories to leave out.  With no filter set, the extensions are the
;; answer, which is what every project that has never thought about it wants.
;;
;; Both halves of sift read this module -- the command line when it walks the
;; operands, and the language server when it indexes the workspace -- so
;; `sift ci .' and `sift lsp' cannot disagree about which files are Lisp files.

(define-module (sift files)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:export (lisp-extensions
            lisp-path?
            compressed-path?
            without-compression
            read-file-text
            skipped-directories
            file-included?
            glob-match?
            patterns->string))

(define lisp-extensions
  '(".scm" ".ss" ".sld" ".sls" ".sps" ".rkt" ".rktd" ".rktl" ".scrbl" ".el"
    ".lisp" ".cl" ".asd" ".lsp" ".system"))

(define (compressed-path? path)
  "Return @code{#t} when @var{path} names a gzip file."
  (and (string? path) (string-suffix? ".gz" path)))

(define (without-compression path)
  "Return @var{path} without the @code{.gz} suffix, when it has one."
  (if (compressed-path? path) (string-drop-right path 3) path))

(define (lisp-path? path)
  "Return @code{#t} when @var{path} ends in an extension sift reads.

`.sps' is an R6RS program and `.sld' an R7RS library: both are Scheme, and a
project that keeps them apart from its `.scm' files would otherwise have its
libraries indexed by everything except the tool meant to read them."
  (let ((dot (string-rindex path #\.)))
    (and dot (member (string-downcase (substring path dot)) lisp-extensions) #t)))

;;; ===== Reading one =====
;;;
;;; A compressed source is read through the decompressor the platform has,
;;; rather than by a second one written here: `gzip' is the program that wrote
;;; the file, and asking it is cheaper and more correct than a deflate
;;; implementation sift would then have to test.  A `gzip' that is not
;;; installed answers nothing, which is the smaller answer rather than a wrong
;;; one, and it is the same answer a file that cannot be read gets.

(define* (decompressed-text path #:optional (limit #f))
  "Return the text @var{path} holds when it is a gzip file, or @code{#f}.

@var{limit} bounds the characters read, so a compressed file that expands to
more than an editor would ever open costs the limit and not the expansion."
  (catch #t
         (lambda ()
           (let ((port (open-pipe* OPEN_READ "gzip" "-dc" "--" path)))
             (dynamic-wind
               (lambda () #f)
               (lambda ()
                 (let ((text (if limit
                               (get-string-n port (+ limit 1))
                               (get-string-all port))))
                   ;; copied before the pipe is closed: a string from a port
                   ;; may share the port's buffer, and closing frees it
                   (and (string? text) (string-copy text))))
               (lambda () (close-pipe port)))))
         (lambda (key . arguments) #f)))

(define* (read-file-text path #:optional (limit #f))
  "Return the text of @var{path}, or @code{#f} when it is not a file to read.

A `.gz' file is decompressed as it is read, so a library that ships compressed
is read like the file beside it.  A file larger than @var{limit} characters,
or one that cannot be read, is not a file this can answer about."
  (let ((text (if (compressed-path? path)
                (decompressed-text path limit)
                (false-if-exception (call-with-input-file path get-string-all)))))
    (and (string? text)
      (or (not limit) (< (string-length text) limit))
      text)))

(define skipped-directories
  '(".git" ".hg" ".svn" ".darcs" "node_modules" "build" "dist" ".cache"
    "_build" "target" "vendor" ".venv" ".tox" ".mypy_cache" "__pycache__"))

;;; ===== Globs =====

(define (glob-pattern pattern)
  "Return a regular expression string matching the glob @var{pattern}.

`*' matches anything but a separator, `**' matches anything at all, and `?'
matches one character.  Everything else stands for itself, which is what makes
a pattern with no wildcard in it a name."
  (let loop ((characters (string->list pattern)) (out '("^")))
    (cond
      [(null? characters) (apply string-append (reverse (cons "$" out)))]
      [(char=? (car characters) #\*)
       (if (and (pair? (cdr characters)) (char=? (cadr characters) #\*))
         (loop (cddr characters) (cons ".*" out))
         (loop (cdr characters) (cons "[^/]*" out)))]
      [(char=? (car characters) #\?)
       (loop (cdr characters) (cons "[^/]" out))]
      [(memv (car characters) '(#\. #\+ #\( #\) #\[ #\] #\{ #\} #\^ #\$ #\|
                                #\\ #\space #\? #\/ #\-))
       (loop (cdr characters)
             (cons (string-append "\\" (string (car characters))) out))]
      [else (loop (cdr characters) (cons (string (car characters)) out))])))

(define (glob-match? pattern path)
  "Return @code{#t} when @var{path} matches the glob @var{pattern}.

A pattern with no separator in it is matched against the file's own name, so
`*.sps' means \"every .sps file, wherever it is\" rather than \"one in the
workspace root\" -- which is what a person writing it means."
  (let* ((subject (if (string-contains pattern "/") path (basename-of path)))
         (regexp (make-regexp (glob-pattern pattern))))
    (and (regexp-exec regexp subject) #t)))

(define (basename-of path)
  (let ((slash (string-rindex path #\/)))
    (if slash (substring path (+ slash 1)) path)))

;;; ===== The filter =====

(define (negated? pattern)
  (string-prefix? "!" pattern))

(define (positive-pattern pattern)
  (if (negated? pattern) (substring pattern 1) pattern))

(define (relative-path root path)
  "Return @var{path} relative to @var{root}, or @var{path} when it is not under it."
  (let ((prefix (string-append (string-trim-right root #\/) "/")))
    (if (string-prefix? prefix path)
      (substring path (string-length prefix))
      path)))

(define* (file-included? patterns path #:optional (root #f))
  "Return @code{#t} when @var{path} is a file sift should read.

@var{patterns} is the project's `file-filter', or @code{#f} for none, in which
case the extension list decides.  With patterns, a path is included when it
matches one of them -- patterns are matched in the order they are written, and
the last one that matches wins, so `!src/generated/**' can be put after the
pattern that let the directory in.

@var{root} is the directory the walk started from: a pattern that names a
directory is written the way a person reads the project, `src/**', not the way
the file system spells it."
  (if (or (not patterns) (null? patterns))
    (lisp-path? path)
    (let ((subject (if root (relative-path root path) path)))
      (let loop ((rest patterns) (verdict #f))
        (cond
          [(null? rest) (and verdict #t)]
          [(glob-match? (positive-pattern (car rest)) subject)
           (loop (cdr rest) (not (negated? (car rest))))]
          [else (loop (cdr rest) verdict)])))))

(define (patterns->string patterns)
  "Return @var{patterns} as one line, for a message."
  (if (or (not patterns) (null? patterns))
    "the built-in extensions"
    (string-join patterns ", ")))
