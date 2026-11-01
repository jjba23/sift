;; sift --- the benchmarks
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; What is timed here is the work a user waits for, in the order they wait for
;; it: read a file, format it, lint it, and the same over a whole tree.  Then
;; the parts those are made of, so that a regression has somewhere to point:
;; every checker is timed on its own, and so are the two node accessors and the
;; line arithmetic that the checkers lean on.
;;
;; The size of the corpus is printed with the table.  A number without the
;; number of lines it was measured over is not a benchmark, it is a memory.

(define-module (benchmarks sift)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift format)
  #:use-module (sift lint)
  #:use-module (sift check base)
  #:use-module (benchmarks measure)
  #:export (spec
            corpus-files
            biggest-file
            corpus-lines
            bench-lines
            checker-benchmarks
            corpus-benchmarks))

;;; ===== The corpus =====

(define (scm-file? name)
  (string-suffix? ".scm" name))

(define (walk directory)
  "Return every Scheme file under @var{directory}, in sorted order."
  (let loop ((entries (or (scandir directory
                                   (lambda (name)
                                     (not (member name '("." ".." ".git")))))
                        '()))
             (out '()))
    (if (null? entries)
      (reverse out)
      (let* ((name (car entries))
             (path (string-append (string-trim-right directory #\/) "/" name))
             (type (catch #t (lambda () (stat:type (stat path)))
                          (lambda (key . args) #f))))
        (loop (cdr entries)
              (cond
                [(not type) out]
                [(eq? type 'directory) (append (reverse (walk path)) out)]
                [(scm-file? name) (cons path out)]
                [else out]))))))

(define (corpus-files)
  "Return the files the whole-tree benchmark runs over."
  (or (and (getenv "SIFT_BENCHMARK_PATHS")
        (append-map walk (string-split (getenv "SIFT_BENCHMARK_PATHS") #\:)))
    (walk "src")))

(define (file-lines path)
  (call-with-input-file path
                        (lambda (port)
                          (let loop ((line (read-line port)) (n 0))
                            (if (eof-object? line) n (loop (read-line port) (+ n 1)))))))

(define (corpus-lines files)
  (apply + (map file-lines files)))

(define* (biggest-file #:optional (files (corpus-files)))
  "Return the largest file of @var{files}, which is what the per-file
benchmarks are timed on: a mean over files of every size says nothing about
either."
  (let ((sized (map (lambda (path)
                      (cons path (catch #t (lambda () (stat:size (stat path)))
                                        (lambda (key . args) 0))))
                    files)))
    (car (car (sort sized (lambda (a b) (> (cdr a) (cdr b))))))))

(define (bench-lines)
  "Return how many lines the per-file benchmarks are timed over."
  (file-lines (biggest-file)))

;;; ===== Per-file work =====

(define (corpus-benchmarks)
  "Return the benchmarks of the whole-tree work: what a pipeline waits for."
  (let* ((files (corpus-files))
         (lines (corpus-lines files)))
    (list
     (list (format #f "lint the tree (~a files, ~a lines)" (length files) lines)
           (lambda () (lint-files files))
           1 0)
     (list (format #f "check the tree (~a files): format" (length files))
           (lambda ()
             (for-each (lambda (path)
                         (let ((source (call-with-input-file path get-string-all)))
                           (format-source source #:dialect 'scheme #:file path)))
                       files))
           1 0)
     (list (format #f "read the tree (~a files)" (length files))
           (lambda ()
             (for-each (lambda (path)
                         (read-source #:source (call-with-input-file path
                                                                     get-string-all)
                                      #:dialect 'scheme #:file path))
                       files))
           1 0))))

;;; ===== One file, and its parts =====

(define (checker-benchmarks)
  "Return one benchmark per checker, timed on the largest file of the corpus.

A checker is timed through the same protocol the driver uses, with the same
options, so a number here is a number of the thing that ships."
  (let* ((path (biggest-file))
         (source (call-with-input-file path get-string-all))
         (dialect 'scheme)
         (ast (call-with-values
                (lambda () (read-source #:source source #:dialect dialect
                                        #:file path))
                (lambda (ast errors) ast))))
    (map (lambda (checker)
           (list (format #f "checker: ~a"
                         (lint-checker-name checker))
                 (lambda ()
                   (with-check-options '()
                                       (lambda ()
                                         ((lint-checker-proc checker)
                                          ast source dialect))))
                 3))
         (checkers-for-dialect dialect))))

(define (file-benchmarks)
  "Return the benchmarks of the per-file work every command does."
  (let* ((path (biggest-file))
         (source (call-with-input-file path get-string-all)))
    (list
     (list (format #f "read (~a)" (basename path))
           (lambda () (read-source #:source source #:dialect 'scheme
                                   #:file path)))
     (list (format #f "format (~a)" (basename path))
           (lambda () (format-source source #:dialect 'scheme #:file path)))
     (list (format #f "lint (~a, every checker)" (basename path))
           (lambda () (lint-source source #:dialect 'scheme #:file path))))))

(define (basename path)
  (let ((slash (string-rindex path #\/)))
    (if slash (substring path (+ slash 1)) path)))

;;; ===== The parts a lint is made of =====

(define (node-benchmarks)
  "Return the benchmarks of the node accessors and the line arithmetic.

These are the two places a pass allocates without doing anything visible:
`node-head-name' is asked once per node per checker, and a line lookup is
asked once per finding per checker.  Timing them apart is what makes a change
to either one measurable."
  (let* ((path (biggest-file))
         (source (call-with-input-file path get-string-all))
         (ast (call-with-values
                (lambda () (read-source #:source source #:dialect 'scheme
                                        #:file path))
                (lambda (ast errors) ast)))
         (nodes (fold-nodes cons '() ast))
         (lists (filter node-list? nodes))
         (indices (map node-index nodes)))
    (list
     (list "node-head-name over every list node"
           (lambda ()
             (for-each node-head-name lists)))
     (list "node-elements over every list node"
           (lambda ()
             (for-each node-elements lists)))
     (list "location-at-index over every node"
           (lambda ()
             (for-each (lambda (index) (location-at-index source index))
                       indices)))
     (list "for-each-form over the tree"
           (lambda ()
             (let ((count 0))
               (for-each-form (lambda (node) (set! count (+ count 1))) ast)
               count))))))

;;; ===== The suite =====

(define (spec)
  "Return the benchmark tasks Veritas runs.

The corpus benchmarks are run once and not warmed up: a whole-tree run is
already several runs' worth of work, and a benchmark that takes a minute to
say `about a second' is not worth running.  Everything else is run three
times after one warm-up, because a single sample of anything under a
millisecond is noise."
  (append (benchmark-tasks (file-benchmarks))
          (benchmark-tasks (node-benchmarks))
          (benchmark-tasks (checker-benchmarks))
          (benchmark-tasks (corpus-benchmarks))))
