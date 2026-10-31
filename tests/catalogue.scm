;; sift --- the rule catalogue
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Three things a catalogue can get wrong, and a test for each: a rule
;; without a code, two rules with the same code, and a document that no longer
;; matches the rules it describes.  The third is the one that matters when
;; someone adds a rule in a hurry.

(define-module (tests catalogue)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift lint)
  #:use-module (sift catalogue)
  #:use-module (tests helpers)
  #:export (spec))

(define (codes-of checkers)
  (map lint-checker-code checkers))

(define (spec)
  (suite "catalogue"

    (test "every rule has a code"
      (assert-equal #:expect '()
        #:got (remove identity (codes-of (all-checkers)))))

    (test "no two rules share a code"
      (let ((codes (codes-of (all-checkers))))
        (assert-equal #:expect (length codes)
          #:got (length (delete-duplicates codes)))))

    (test "every code looks like one"
      (assert-equal #:expect '()
        #:got (remove (lambda (code)
                        (and (string? code)
                          (string-prefix? "SIFT" code)
                          (= 7 (string-length code))))
                (codes-of (all-checkers)))))

    (test "every rule has an example and a sentence about its fix"
      (assert-equal #:expect '()
        #:got (remove (lambda (checker)
                        (let ((example (lint-checker-example checker)))
                          (and example
                            (assq-ref example 'input)
                            (assq-ref example 'fix))))
                (all-checkers))))

    (test "the document names every rule by name and code"
      (let ((document (catalogue->org)))
        (for-each (lambda (checker)
                    (assert-contains #:expect document
                      #:got (format #f "** ~a ~a"
                              (lint-checker-code checker)
                              (lint-checker-name checker))))
          (all-checkers))))

    (test "doc/rules.org is what the rules produce"
      ;; the drift check: add a rule without `maak docs' and this fails
      (assert-true (catalogue-check #:path (string-append (test-root)
                                             "/doc/rules.org"))
        #:name "doc/rules.org is up to date"))

    (test "the summary table has a row per rule"
      (let* ((document (catalogue->org))
             (rows (filter (lambda (line)
                             (and (string-prefix? "| " line)
                               (string-contains line "SIFT")))
                     (string-split document #\newline))))
        (assert-equal #:expect (length (all-checkers)) #:got (length rows))))))
