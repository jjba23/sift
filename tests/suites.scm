;; sift --- the suite about the suites
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A test that is not a direct child of a suite is a test that never runs, and
;; Veritas reports the run as green either way.  That happened once: a missing
;; parenthesis made a `test` form part of the test above it, so two assertions
;; were counted as passing by never executing.  This suite reads the test files
;; with sift's own reader -- which knows where every form begins -- and says so,
;; so the next edit that loses a parenthesis fails a test instead of shrinking
;; the suite quietly.
;;
;; It also guards the guard: the walk has to find the files and the tests it
;; is about, or an empty answer would pass for a clean one.

(define-module (tests suites)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift check base)
  #:use-module (tests helpers)
  #:export (spec))

;;; ===== The files =====

(define (file-under? name)
  (not (member name '("." ".."))))

(define (collect path)
  "Return the Scheme files under @var{path}, fixtures aside."
  (if (eq? (stat:type (stat path)) 'directory)
    (append-map (lambda (name) (collect (string-append path "/" name)))
                (scandir path file-under?))
    (if (and (string-suffix? ".scm" path)
          (not (string-contains path "/fixtures/")))
      (list path)
      '())))

(define (suite-files)
  "Return the test files, sorted, the support modules aside."
  (sort (remove (lambda (path)
                  (member (basename path) '("helpers.scm" "run.scm" "suites.scm")))
                (collect (string-append (test-root) "/tests")))
        string<?))

;;; ===== Reading one file =====

(define (forms-of path)
  (call-with-values (lambda () (read-source/file path #:dialect 'scheme))
    (lambda (ast errors)
      (values ast errors))))

(define (head-name-of node)
  (and (node-list? node) (node-head-name node)))

(define (named-form? node name)
  (eq? (head-name-of node) name))

(define (place path node)
  (format #f "~a:~a"
          (basename path)
          (source-location-line (node-start node))))

(define (collect-forms ast want)
  "Return @code{(node . ancestors)} for every form headed by @var{want}."
  (let ((found '()))
    (walk-forms (lambda (node ancestors)
                  (when (named-form? node want)
                    (set! found (cons (cons node ancestors) found))))
                ast)
    (reverse found)))

;;; ===== The rules =====

(define (test-outside-a-suite path ast)
  "Return a sentence for every test that is not a direct child of a suite."
  (filter-map
   (lambda (entry)
     (let ((node (car entry))
           (ancestors (cdr entry)))
       (and (not (and (pair? ancestors) (named-form? (car ancestors) 'suite)))
         (format #f "~a: a test inside ~a never runs"
                 (place path node)
                 (let ((parent (and (pair? ancestors) (car ancestors))))
                   (if parent
                     (format #f "`~a`" (head-name-of parent))
                     "the file"))))))
   (collect-forms ast 'test)))

(define (suite-outside-spec path ast)
  "Return a sentence for every suite a `spec` does not build."
  (filter-map
   (lambda (entry)
     (let ((node (car entry))
           (ancestors (cdr entry)))
       (cond
         [(any (lambda (ancestor) (named-form? ancestor 'test)) ancestors)
          (format #f "~a: a suite inside a test never runs" (place path node))]
         [(not (any (lambda (ancestor)
                      (and (named-form? ancestor 'define)
                        (let ((name (and (pair? (cdr (node-elements ancestor)))
                                      (cadr (node-elements ancestor)))))
                          (and name (named-form? name 'spec)))))
                    ancestors))
          (format #f "~a: a suite no `spec` builds" (place path node))]
         [else #f])))
   (collect-forms ast 'suite)))

(define (specs-in ast)
  (filter (lambda (node)
            (and (named-form? node 'define)
              (let ((name (and (pair? (cdr (node-elements node)))
                            (cadr (node-elements node)))))
                (and name (named-form? name 'spec)))))
          ast))

(define (spec)
  (suite "the suites themselves"

    (test "the walk finds the test files, and the tests in them"
      ;; a meta suite that silently found nothing would pass for a clean one
      (assert-true (> (length (suite-files)) 100)
        #:name (format #f "~a test files" (length (suite-files))))
      (assert-true (> (fold + 0
                            (map (lambda (path)
                                   (call-with-values (lambda () (forms-of path))
                                     (lambda (ast errors)
                                       (length (collect-forms ast 'test)))))
                                 (suite-files)))
                      1000)
        #:name "more than a thousand tests"))

    (test "every test is a direct child of a suite"
      (assert-equal #:expect '()
        #:got (append-map (lambda (path)
                            (call-with-values (lambda () (forms-of path))
                              (lambda (ast errors)
                                (test-outside-a-suite path ast))))
                          (suite-files))))

    (test "every suite is built by a `spec`, and never inside a test"
      (assert-equal #:expect '()
        #:got (append-map (lambda (path)
                            (call-with-values (lambda () (forms-of path))
                              (lambda (ast errors)
                                (suite-outside-spec path ast))))
                          (suite-files))))

    (test "every suite file has a `spec` to ask"
      (assert-equal #:expect '()
        #:got (filter-map (lambda (path)
                            (call-with-values (lambda () (forms-of path))
                              (lambda (ast errors)
                                (and (null? (specs-in ast)) (basename path)))))
                          (suite-files))))

    (test "every suite file reads without a reader finding"
      (assert-equal #:expect '()
        #:got (filter-map (lambda (path)
                            (call-with-values (lambda () (forms-of path))
                              (lambda (ast errors)
                                (and (pair? errors)
                                  (format #f "~a: ~a finding(s)"
                                          (basename path) (length errors))))))
                          (suite-files))))))
