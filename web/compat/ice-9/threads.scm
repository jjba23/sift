;; sift --- `(ice-9 threads)' for a WebAssembly build
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A WebAssembly module has one thread unless it is built with shared memory
;; and atomics, and Hoot is not: there is no `make-mutex', no
;; `call-with-new-thread' and nothing to join.  What there *is* is
;; `(sift pool)', which imports `(ice-9 threads)' for the whole file -- so the
;; import has to resolve even though the browser will only ever take its
;; sequential path.
;;
;; That is the point of this file, and the reason it answers one and refuses
;; the rest:
;;
;;   `current-processor-count'   one, which is the truth.
;;
;; Everything else raises.  `(sift pool)' asks for the processor count first
;; and does the work in the calling thread when the answer is one, so a build
;; that reached a mutex would be a build whose pool decided to parallelise
;; against the evidence -- and a browser tab that hangs on a thread that can
;; never be joined is a worse failure than one that says why, in a sentence,
;; with the name of the procedure it wanted.
;;
;; Nothing in sift calls these at load time, so a WebAssembly build starts.

(define-library (ice-9 threads)
  (export current-processor-count
    total-processor-count
    make-mutex
    make-recursive-mutex
    mutex?
    lock-mutex
    unlock-mutex
    call-with-new-thread
    join-thread
    thread?
    all-threads
    current-thread)
  (import (scheme base)
    (scheme write))

  (begin
    (define (unsupported name)
      (error (string-append
              "sift: (ice-9 threads) is not available in a WebAssembly build, "
              "so " name " cannot be called")
             name))

    (define (current-processor-count)
      "Return 1: a WebAssembly module is one thread."
      1)

    (define (total-processor-count)
      "Return 1: a WebAssembly module is one thread."
      1)

    (define (make-mutex . rest) (unsupported "make-mutex"))
    (define (make-recursive-mutex . rest) (unsupported "make-recursive-mutex"))
    (define (mutex? . rest) (unsupported "mutex?"))
    (define (lock-mutex . rest) (unsupported "lock-mutex"))
    (define (unlock-mutex . rest) (unsupported "unlock-mutex"))
    (define (call-with-new-thread . rest) (unsupported "call-with-new-thread"))
    (define (join-thread . rest) (unsupported "join-thread"))
    (define (thread? . rest) (unsupported "thread?"))
    (define (all-threads . rest) (unsupported "all-threads"))
    (define (current-thread . rest) (unsupported "current-thread"))))
