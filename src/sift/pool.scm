;; sift --- work spread over the processors
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A run over a project is a run over one file at a time, and the files do not
;; know about each other: nothing a checker reads in one file is anything it
;; reads in another.  That is exactly the shape `map-ordered' takes.
;;
;; Two things are promised here and are what make it safe to use:
;;
;;   * the answers come back in the order the work was given, so a parallel run
;;     prints the same bytes as a sequential one, and
;;   * an error is re-raised in the caller's thread, in that order, so a
;;     caller's `catch' reads the same either way.
;;
;; Guile's threads are real threads in Guile 3, and the garbage collector stops
;; them all -- so a lint that allocates less also parallelises better, which is
;; the other reason the allocation numbers in the benchmarks are worth keeping
;; an eye on.

(define-module (sift pool)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 threads)
  #:export (processor-count
            worker-count
            map-ordered
            for-each-in-order))

;;; ===== How many workers =====

(define (processor-count)
  "Return how many processors this run may use, at least one."
  (max 1 (or (current-processor-count) 1)))

(define (worker-count total)
  "Return how many threads to use for @var{total} items.

`SIFT_JOBS' overrides the count, which is what makes a parallel run
comparable with a sequential one: `SIFT_JOBS=1 sift ci .' does the same work in
the same order on one thread, so a difference in the output is a bug and not a
guess.

More workers than items is a thread that takes no work and still has to be
joined, and a machine with more processors than a run has files does not need
all of them: the work here is a few hundred milliseconds, and the point is to
stop waiting, not to saturate a machine."
  (or (and (getenv "SIFT_JOBS")
        (let ((n (string->number (getenv "SIFT_JOBS"))))
          (and n (exact? n) (> n 0) (inexact->exact n))))
    (max 1 (min total (processor-count)))))

;;; ===== The map =====

(define* (map-ordered proc items #:key (count #f))
  "Apply @var{proc} to every item of @var{items}, answering in order.

The items are handed out one at a time from a shared counter, so a list of one
large file and twenty small ones takes as long as the workers can make it
rather than as long as the slowest bucket would have; and the answers are
collected by index, so the order is the order they were given in.

With one item, one processor, or @var{count} of one, the work is done in the
calling thread: a thread that exists to be joined is a promise to be kept for
nothing."
  (let* ((items (list->vector items))
         (total (vector-length items))
         (count (or count (worker-count total))))
    (cond
      [(or (<= count 1) (<= total 1))
       (let loop ((i 0) (out '()))
         (if (>= i total)
           (reverse! out)
           (loop (+ i 1) (cons (proc (vector-ref items i)) out))))]
      [else
        (let* ((count (min count total))
               (results (make-vector total #f))
               (next (vector 0))
               (lock (make-mutex))
               (take!
                (lambda ()
                  "Return the next unclaimed index, or @code{#f} when there is none."
                  (lock-mutex lock)
                  (let ((i (vector-ref next 0)))
                    (if (>= i total)
                      (begin (unlock-mutex lock) #f)
                      (begin (vector-set! next 0 (+ i 1))
                        (unlock-mutex lock)
                        i)))))
               (run
                (lambda ()
                  (let loop ()
                    (let ((i (take!)))
                      (when i
                        (vector-set! results i
                                     (catch #t
                                            (lambda ()
                                              (cons 'answered
                                                    (proc (vector-ref items i))))
                                            (lambda (key . args)
                                              (cons 'raised (cons key args)))))
                        (loop))))))
               (threads (map (lambda (ignored) (call-with-new-thread run))
                             (iota count))))
          (for-each join-thread threads)
          (let loop ((i 0) (out '()))
            (if (>= i total)
              (reverse! out)
              (let ((result (vector-ref results i)))
                (unless result
                  (error "sift: an item was never taken by a worker" i))
                (case [car result]
                  [(answered) (loop (+ i 1) (cons (cdr result) out))]
                  [else (apply throw (cdr result))])))))])))

(define* (for-each-in-order proc items #:key (count #f))
  "Call @var{proc} on every item of @var{items}, in parallel, discarding answers."
  (map-ordered proc items #:count count)
  *unspecified*)
