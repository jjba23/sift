;; sift --- benchmark runner
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `maak bench' loads this, which loads `(benchmarks sift)' and hands the tasks
;; its `spec' returns to one `veritas-run' -- the same runner the test suite
;; uses, with the same reporter, so a benchmark is printed by the thing that
;; prints everything else rather than by a second, private printer.
;;
;; The reporter prints one block per benchmark; the summary table is printed
;; after it, sorted worst-first, because the question a benchmark run answers
;; is `where does the time go', and that is a table, not a list in the order
;; the benchmarks happen to be written in.
;;
;; Nothing here decides pass or fail: a benchmark says how long something took,
;; and whether that is good is a question about the previous run, which this
;; run has not seen.  The exit status is 0 unless the benchmarks could not be
;; built at all, which is the one failure a benchmark run can have.

(define-module (benchmarks run)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (benchmarks sift)
  #:use-module (benchmarks measure)
  #:export (run-benchmarks!))

(define (run-benchmarks!)
  "Run every benchmark and report it; return the exit status."
  (catch #t
         (lambda ()
           (format (current-error-port)
                   "sift benchmarks: ~a file(s), the biggest ~a line(s)~%"
                   (length (corpus-files)) (bench-lines))
           (forget-measurements!)
           ;; Benchmarks are run one at a time: a mean taken while another
           ;; benchmark is running is a mean of the contention, not of the code.
           (apply veritas-run #:shuffle? #f #:concurrent? #f (spec))
           (report-table)
           0)
         (lambda (key . args)
           (format (current-error-port) "sift benchmarks: ~a ~s~%" key args)
           1)))
