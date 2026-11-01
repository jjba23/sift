;; sift --- work spread over the processors
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A parallel map is only worth having if it answers the same thing the
;; sequential one does, in the same order.  These are the two promises:
;; the answers come back in the order the work was given, and an error comes
;; back to the caller rather than disappearing into a thread.

(define-module (tests pool)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:use-module (sift pool)
  #:export (spec))

(define (deep n)
  "Do some work whose answer says which item did it."
  (let loop ((i 0) (acc 0))
    (if (>= i n)
      acc
      (loop (+ i 1) (+ acc (modulo i 7))))))

(define (sleepy n)
  "Do the same work after a delay chosen by the item, so the order of finishing
is the opposite of the order of starting."
  (let loop ((i 0))
    (when (< i (* 2000 (- 8 n)))
      (loop (+ i 1))))
  (* n n))

(define (spec)
  (suite "pool: many files, one order"

    (test "the answers are in the order the work was given"
      ;; the work is deliberately slowest first, so a map that answered in
      ;; completion order would answer backwards
      (assert-equal #:expect '(0 1 4 9 16 25 36 49)
        #:got (map-ordered sleepy (iota 8) #:count 8)
        #:name "the order is the order of the items"))

    (test "one worker answers what many workers answer"
      (let ((items (iota 40)))
        (assert-equal #:expect (map-ordered deep items #:count 1)
          #:got (map-ordered deep items #:count 8)
          #:name "the same answers, however they were spread")))

    (test "the empty list is answered without a thread"
      (assert-equal #:expect '()
        #:got (map-ordered deep '() #:count 8)))

    (test "an error reaches the caller instead of vanishing in a thread"
      (assert-error
        (lambda ()
          (map-ordered (lambda (n)
                         (if (= n 5) (error "no" n) n))
                       (iota 10) #:count 4))
        #:name "the caller sees the error the work raised"))

    (test "the error is the one the work raised, not a summary of it"
      (assert-equal #:expect '(0 1 2)
        #:got (catch 'a-sift-test
                     (lambda ()
                       (map-ordered (lambda (n)
                                      (if (= n 3) (throw 'a-sift-test n) n))
                                    (iota 6) #:count 3)
                       'no-error)
                     (lambda (key n) (iota n)))
        #:name "the key and its arguments arrive unchanged"))))
