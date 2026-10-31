;; sift --- test runner
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Every file under tests/ that is a suite defines @code{(spec)}, a procedure
;; returning the Veritas tasks it wants run.  This runner loads them all and
;; hands the whole list to one `veritas-run', so `maak test' reports one
;; pass/fail summary for the project rather than one per file, and a failing
;; suite still names its file, its suite and its assertion.
;;
;; Files are discovered by walking tests/ in sorted order, so a run is
;; reproducible; fixtures and the two support modules are skipped.

(define-module (tests run)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (veritas veritas)
  #:export (test-files
              test-modules
              run-all!))

;;; ===== Discovery =====

(define support-files
  '("tests/helpers.scm" "tests/run.scm"))

(define (scm-file? name)
  (string-suffix? ".scm" name))

(define (test-files)
  "Return the suite files, relative to the repository root, in sorted order."
  (filter (lambda (path)
            (and (scm-file? path)
              (not (member path support-files))
              (not (string-contains path "/fixtures/"))))
    (sort (find-files* "tests") string<?)))

(define (find-files* directory)
  (let ((entries (scandir directory
                   (lambda (name)
                     (not (member name '("." ".." ".git")))))))
    (append-map
       (lambda (name)
         (let ((path (string-append directory "/" name)))
           (cond [(eq? (stat:type (stat path)) 'directory)
                  (find-files* path)]
                 [(scm-file? name) (list path)]
                 [else '()])))
       entries)))

(define (module-name-of path)
  "Turn tests/checkers/tabs.scm into (tests checkers tabs)."
  (map string->symbol
    (string-split (string-drop-right path 4) #\/)))

;;; ===== Running =====

(define (spec-tasks spec)
  "Return the tasks @var{spec} asks for, as a list.

A file with one suite returns that suite; a file with several returns a list
of them.  Both are understood, and neither can be dropped: an earlier runner
wrapped whatever it was given in another list, so a file that returned two
suites had both of them silently skipped while the run still reported a
total."
  (let ((tasks (spec)))
    (if (list? tasks) tasks (list tasks))))

(define (load-suites path)
  "Load the module for @var{path} and return the tasks its spec asks for."
  (let* ((module (resolve-module (module-name-of path)))
         (spec (module-ref module 'spec)))
    (spec-tasks spec)))

(define (run-all!)
  "Run every suite; return the exit status Veritas would exit with.

A file that fails to load is reported as a failure rather than being skipped:
a suite that cannot be read has not passed."
  (let* ((files (test-files))
         (tasks '())
         (broken '()))
    (for-each
       (lambda (path)
         (format (current-error-port) "loading ~a~%" path)
         (catch #t
           (lambda () (set! tasks (append tasks (load-suites path))))
           (lambda (key . args)
             (set! broken (cons (format #f "~a: ~a ~s" path key args) broken)))))
       files)
    (for-each (lambda (message)
                (format (current-error-port) "sift: could not load ~a~%" message))
      (reverse broken))
    (cond
      [(null? tasks)
       (format (current-error-port) "sift: no test suites were found~%")
       1]
      ;; A suite is a procedure, so a list where a suite should be means a
      ;; suite was never unwrapped: Veritas would then run nothing and report
      ;; success, which is the one outcome a test runner must never give.
      [(any list? tasks)
       (format (current-error-port)
         "sift: a suite was passed to the runner without being unwrapped~%")
       1]
      [else
         (apply veritas-run #:shuffle? #f #:concurrent? #f tasks)
         (if (null? broken) 0 1)])))
