;; sift --- benchmark harness
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Veritas is the runner here too, but not its `benchmark' form: that one
;; hands every run to Guile's `statprof', whose SIGPROF handler is
;; process-global and whose answer is a profile rather than a number.  What a
;; change to the reader or to a checker needs is the number -- how many
;; milliseconds and how many bytes the same work costs before and after -- so a
;; benchmark here is a `benchmark-result' built from our own timing, which the
;; Veritas reporter then prints with the rest of the results.
;;
;; Two numbers are reported, and both matter:
;;
;;   * the wall clock, which is what a user waits for, and
;;   * `heap-total-allocated', which is what the garbage collector has to
;;     collect.  A pass that allocates a list per node is a pass that makes
;;     every other pass slower, and the allocation count is how that shows up
;;     even when the timing happens to look fine.
;;
;; A benchmark is timed after one untimed warm-up run, because the first run
;; of anything in Guile pays for the module's own lazy initialisation, and the
;; whole table is printed once at the end: per-run detail is what the reporter
;; prints, and a table to scan is what a person wants.

(define-module (benchmarks measure)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (veritas model)
  #:export (<measurement>
            measurement-name
            measurement-runs
            measurement-samples
            measurement-min
            measurement-median
            measurement-mean
            measurement-bytes
            measurement?
            measure
            measured
            measurements
            forget-measurements!
            measurement->string
            bytes->string
            bench
            benchmark-tasks
            report-table))

;;; ===== Timing =====

(define-record-type <measurement>
  (%make-measurement name runs samples)
  measurement?
  (name measurement-name) ; string
  (runs measurement-runs) ; the number of timed runs
  (samples measurement-samples)) ; list of (ms bytes), one per run

(define (measurement-min m)
  "Return the fastest of @var{m}'s runs, in milliseconds."
  (apply min (map car (measurement-samples m))))

(define (measurement-median m)
  "Return the middle of @var{m}'s runs, in milliseconds."
  (let* ((times (sort (map car (measurement-samples m)) <))
         (n (length times)))
    (cond
      [(zero? n) 0]
      [(odd? n) (list-ref times (quotient n 2))]
      [else (/ (+ (list-ref times (- (quotient n 2) 1))
                  (list-ref times (quotient n 2)))
               2.0)])))

(define (measurement-mean m)
  "Return the mean of @var{m}'s runs, in milliseconds."
  (let ((times (map car (measurement-samples m))))
    (if (null? times) 0 (/ (apply + times) (exact->inexact (length times))))))

(define (measurement-bytes m)
  "Return the most bytes any one of @var{m}'s runs allocated."
  (if (null? (measurement-samples m))
    0
    (apply max (map cadr (measurement-samples m)))))

;; How many internal time units make a millisecond.
(define internal-ms (/ internal-time-units-per-second 1000.0))

(define (now)
  "Return the current real time, in internal units."
  (get-internal-real-time))

(define (allocated)
  "Return how many bytes this process has allocated so far, including GC'd."
  (or (assq-ref (gc-stats) 'heap-total-allocated) 0))

(define (timed thunk)
  "Run @var{thunk} and return @code{(ms bytes)}, the cost of that one run."
  (let* ((before-bytes (allocated))
         (before (now))
         (value (thunk))
         (after (now))
         (after-bytes (allocated)))
    (list (exact->inexact (/ (- after before) internal-ms))
          (- after-bytes before-bytes)
          value)))

(define* (measure name thunk #:key (runs 5) (warmup 1))
  "Time @var{thunk} @var{runs} times and return a @code{<measurement>}.

One warm-up run is made first and not counted: the first call of anything in
Guile pays for lazy initialisation that a second call does not, and a mean
that includes it says more about the harness than about the code."
  (let loop ((left warmup))
    (when (> left 0)
      (thunk)
      (loop (- left 1))))
  (let ((m (%make-measurement
            name runs
            (map (lambda (ignored)
                   (let ((sample (timed thunk)))
                     (list (car sample) (cadr sample))))
                 (iota runs)))))
    (measured m)
    m))

;; Every measurement of the run, in the order they were taken, so that the
;; table can be printed once at the end rather than in the middle of the
;; reporter's own output.
(define *measurements* '())

(define (measured m)
  "Remember @var{m} so that the table can print it at the end of the run."
  (set! *measurements* (cons m *measurements*))
  m)

(define (measurements)
  "Return every measurement taken so far, in the order they were taken."
  (reverse *measurements*))

(define (forget-measurements!)
  "Forget every measurement taken so far."
  (set! *measurements* '()))

;;; ===== Rendering =====

(define (bytes->string bytes)
  "Render a byte count in the unit a person reads it in."
  (cond
    [(>= bytes (* 1024 1024))
     (format #f "~,1f MiB" (/ bytes (* 1024.0 1024.0)))]
    [(>= bytes 1024) (format #f "~,1f KiB" (/ bytes 1024.0))]
    [else (format #f "~a B" bytes)]))

(define (ms->string ms)
  (format #f "~ams" (inexact->exact (round ms))))

(define (measurement->string m)
  "Render @var{m} as one line of a benchmark table."
  (format #f "~a ~a (min ~a, median ~a), ~a allocated"
          (string-pad-right (measurement-name m) 40)
          (string-pad (ms->string (measurement-mean m)) 8)
          (string-pad (ms->string (measurement-min m)) 8)
          (string-pad (ms->string (measurement-median m)) 8)
          (bytes->string (measurement-bytes m))))

(define (report-table)
  "Print every measurement of the run as one aligned table, cheapest last."
  (let ((all (sort (measurements)
                   (lambda (a b) (> (measurement-mean a) (measurement-mean b))))))
    (format #t "~%sift benchmarks~%")
    (format #t "~a~%" (make-string 78 #\-))
    (for-each (lambda (m) (format #t "~a~%" (measurement->string m))) all)
    (format #t "~a~%" (make-string 78 #\-))
    (format #t "~a measurement(s)~%~%" (length all))))

;;; ===== Tasks =====

(define* (bench name thunk #:key (runs 5) (warmup 1))
  "Return a Veritas task that measures @var{thunk} and reports it as a benchmark.

The task is a procedure of one argument, the validation client Veritas hands
every task, so it can be put in the list a @code{spec} returns beside tests
and suites and be run by the same runner."
  (lambda (vc)
    (let ((m (measure name thunk #:runs runs #:warmup warmup)))
      (make-benchmark-result
       name
       (map (lambda (index sample)
              `((run-index . ,index)
                (run-details . ,(format #f "~a, ~a allocated"
                                        (ms->string (car sample))
                                        (bytes->string (cadr sample))))))
            (iota runs)
            (measurement-samples m))))))

(define (benchmark-tasks entries)
  "Return the benchmark tasks for @var{entries}, a list of entries.

Each entry is @code{(name thunk)}, @code{(name thunk runs)} or
@code{(name thunk runs warmup)}.  The shape is deliberately plain rather than
another macro: a benchmark suite is a table of what is being timed, and a
table is something a reader can scan without knowing how the harness is
spelled."
  (map (lambda (entry)
         (bench (car entry) (cadr entry)
                #:runs (if (pair? (cddr entry)) (caddr entry) 5)
                #:warmup (if (and (pair? (cddr entry))
                               (pair? (cdddr entry)))
                           (cadddr entry)
                           1)))
       entries))
