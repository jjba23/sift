;; sift --- the dialects, and everything that has to know about them
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A dialect is not one line in a record.  It is a name an editor can send, a
;; file extension, a set of delimiters, an indentation table, a standard
;; library, and a column in four documents -- and a contributor who adds one
;; and misses a place is not told, because what the tool does with an unknown
;; dialect is fall back to Scheme's answer.  A file in a new dialect would be
;; read as Scheme, formatted with Scheme's table and skipped by every rule
;; that names its dialects, quietly.
;;
;; This suite is the telling.  Each test names the table the dialect is
;; missing from, so adding a dialect is a matter of running the suite until it
;; is quiet rather than of remembering the list.  What it cannot check is the
;; part that is a decision: which syntax the dialect's reader has, and which
;; rules apply to it.  Those are written down in doc/contributing.org and are
;; the work.

(define-module (tests dialects)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift format)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "dialects"

    (test "every dialect can be named, and named back"
      ;; the name an editor sends, the name a message prints: a dialect in the
      ;; record that `string->dialect' does not know is one `--dialect' cannot
      ;; reach and the browser selector cannot offer
      (for-each
       (lambda (dialect)
         (assert-equal #:expect dialect
           #:got (string->dialect (dialect->string dialect))
           #:name (format #f "~a round-trips through its own name" dialect)))
       dialects))

    (test "every dialect has a file extension"
      (for-each
       (lambda (dialect)
         (let ((row (assq dialect dialect-extensions)))
           (assert-true (and row (pair? (cdr row)))
             #:name (format #f "~a claims an extension in dialect-extensions"
                            dialect))
           (when (and row (pair? (cdr row)))
             (assert-equal #:expect dialect
               #:got (extension-dialect (string-append "example"
                                                       (car (cdr row))))
               #:name (format #f "a ~a file is detected as ~a"
                              (car (cdr row)) dialect)))))
       dialects))

    (test "every dialect has an indentation table"
      (for-each
       (lambda (dialect)
         (let ((row (assq dialect dialect-indent-tables)))
           (assert-true (and row (pair? (cdr row)))
             #:name (format #f "~a has a row in dialect-indent-tables"
                            dialect))
           (assert-true (and (pair? (indent-rules dialect)) #t)
             #:name (format #f "~a's indentation table is not empty"
                            dialect))))
       dialects))

    (test "every dialect has a default bracket style and line length"
      (for-each
       (lambda (dialect)
         (assert-true (if (default-brackets dialect) #t #f)
           #:name (format #f "~a has a default bracket style" dialect))
         (assert-true (> (default-line-length dialect) 0)
           #:name (format #f "~a has a default line length" dialect)))
       dialects))

    (test "the documents list every dialect"
      ;; the two pages a reader opens to find out what sift reads.  `cl' is too
      ;; short to look for in prose, so the pages are asked for the spelling
      ;; each one uses.
      (let ((spellings
             '((scheme . "Scheme") (racket . "Racket")
               (elisp . "Emacs Lisp") (cl . "Common Lisp"))))
        (for-each
         (lambda (page)
           (let ((text (slurp (string-append (test-root) "/" page))))
             (for-each
              (lambda (dialect)
                (assert-true (if (string-contains text
                                                  (assq-ref spellings dialect))
                               #t #f)
                  #:name (format #f "~a names ~a" page dialect)))
              dialects)))
         '("README.org" "web/index.html"))))))
