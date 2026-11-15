;; sift --- the rule catalogue
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Three things a catalogue can get wrong, and a test for each: a rule
;; without a code, two rules with the same code, and a document that no longer
;; matches the rules it describes.  The third is the one that matters when
;; someone adds a rule in a hurry.
;;
;; The last three tests are the other half of that: a rule is a record, a
;; checker in a family module, a test file of its own, and a count that four
;; documents state.  `doc/contributing.org' lists the steps; these are the ones
;; that fail when a step is skipped, so the list does not have to be
;; remembered.

(define-module (tests catalogue)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift lint)
  #:use-module (sift check base)
  #:use-module (sift catalogue)
  #:use-module (tests helpers)
  #:export (spec))

(define (codes-of checkers)
  (map lint-checker-code checkers))

(define (rule-test-files)
  "Return the rule test files, sorted."
  (let ((directory (string-append (test-root) "/tests/checkers")))
    (map (lambda (name) (string-append directory "/" name))
         (sort (scandir directory (lambda (name) (string-suffix? ".scm" name)))
               string<?))))

(define (symbol-atoms-of path)
  "Return every name written as a symbol in @var{path}.

A comment is not an atom, so naming a rule in prose does not count as testing
it -- which is the whole point of the check: the test has to mention the rule
where the linter can see it."
  (call-with-values
    (lambda () (read-source/file path #:dialect 'scheme))
    (lambda (ast errors)
      (let ((names '()))
        (for-each-node
         (lambda (node)
           (when (and (node-atom? node) (eq? (node-atom-type node) 'symbol))
             (set! names (cons (node-value node) names))))
         ast)
        names))))

(define (rules-named-in-tests)
  "Return the names the rule tests mention, once each."
  (delete-duplicates (append-map symbol-atoms-of (rule-test-files)) string=?))

(define (rule-counts-in text)
  "Return the rule counts @var{text} states, as strings.

`95 rules' is how every page in this repository says it, and the number is the
catalogue's own length -- a page that says another number is a page written
before a rule existed, which is the documentation defect that is nobody's
fault and everybody's."
  (let ((pattern (make-regexp "([0-9]+) rules")))
    (let loop ((start 0) (found '()))
      (let ((match (regexp-exec pattern text start)))
        (if match
          (loop (match:end match 0) (cons (match:substring match 1) found))
          (reverse found))))))

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
        (assert-equal #:expect (length (all-checkers)) #:got (length rows))))

    ;; --- what a rule has to reach --------------------------------------
    ;;
    ;; A rule is one record and one test file, and the catalogue, the
    ;; documentation and the editor integrations follow from them.  The record
    ;; is checked above; these are the two that a contributor writing a rule in
    ;; a hurry skips, and the count that four documents state.

    (test "every rule is named by a test under tests/checkers"
      (let ((named (rules-named-in-tests)))
        (assert-equal #:expect '()
          #:got (filter-map (lambda (checker)
                              (let ((name (symbol->string
                                           (lint-checker-name checker))))
                                (and (not (member name named)) name)))
                            (all-checkers))
          #:name "every rule is exercised somewhere, not only written down")))

    (test "no rule applies to a dialect sift does not read"
      (assert-equal #:expect '()
        #:got (delete-duplicates
               (append-map (lambda (checker)
                             (let ((applies (lint-checker-dialects checker)))
                               (if (eq? applies 'all)
                                 '()
                                 (remove valid-dialect? applies))))
                           (all-checkers)))
        #:name "a dialect list holds names from the record, not a guess"))

    (test "the documents state the number of rules there are"
      (let ((count (number->string (length (all-checkers)))))
        (for-each
         (lambda (page)
           (let ((claims (rule-counts-in
                          (slurp (string-append (test-root) "/" page)))))
             (assert-true (pair? claims)
               #:name (format #f "~a says how many rules there are" page))
             (assert-equal #:expect (map (lambda (claim) count) claims)
               #:got claims
               #:name (format #f "~a's count is the catalogue's" page))))
         '("README.org" "doc/linting.org" "doc/rules.org"
           "web/README.org" "VISION.md"))))))
