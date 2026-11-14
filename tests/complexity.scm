;; sift --- measuring a definition
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Three places ask how much a definition asks of a reader: `maak complexity'
;; prints the ranking, and two rules report one definition at a time.  They
;; ask one measurement, and this file is what that means: a branch costs one,
;; a branch inside a branch costs one more, a path is a decision plus the way
;; in, and a datum -- quoted, commented out, or a string -- is not a branch.

(define-module (tests complexity)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift complexity)
  #:use-module (tests helpers)
  #:export (spec))

(define* (source-forms source #:key (dialect 'scheme))
  (call-with-values
    (lambda () (read-source-string source #:dialect dialect))
    (lambda (ast errors) ast)))

(define* (measure-first source #:key (dialect 'scheme))
  "Return the measurement of the first definition in @var{source}."
  (complexity-of (find complexity-definition?
                       (source-forms source #:dialect dialect))))

(define (spec)
  (suite "complexity: measuring a definition"

    (test "a branch costs one point and one more path"
      (let ((measure (measure-first "(define (f x)\n  (if (ready? x) 1 2))\n")))
        (assert-equal #:expect 1 #:got (complexity-score measure))
        (assert-equal #:expect 2 #:got (complexity-cycles measure))
        (assert-equal #:expect 1 #:got (complexity-depth measure))))

    (test "a branch inside a branch costs one more than the branch around it"
      (let ((measure (measure-first
                      "(define (f x)\n  (if (a) (if (b) 1 2) 3))\n")))
        (assert-equal #:expect 3 #:got (complexity-score measure)
          #:name "one point for the outer branch and two for the inner one")
        (assert-equal #:expect 3 #:got (complexity-cycles measure))
        (assert-equal #:expect 2 #:got (complexity-depth measure))))

    (test "a quoted datum is data rather than a branch"
      ;; the alist key of a pattern is not a decision the reader makes
      (let ((measure (measure-first
                      "(define pattern '(if a (cond [b 1])))\n")))
        (assert-equal #:expect 0 #:got (complexity-score measure))
        (assert-equal #:expect 1 #:got (complexity-cycles measure))))

    (test "a datum comment is not code"
      (let ((measure (measure-first
                      "(define (f)\n  #;(if a 1 2)\n  (if b 1 2))\n")))
        (assert-equal #:expect 1 #:got (complexity-score measure))))

    (test "a nested definition is its own unit of reading"
      (let* ((source "(define (outer)\n  (define (inner)\n    (if a (if b 1 2)))\n  (if c 1 2))\n")
             (outer (car (source-forms source)))
             (inner (list-ref (node-elements outer) 2)))
        (assert-equal #:expect 1
          #:got (complexity-score (complexity-of outer))
          #:name "the inner definition is not folded into the outer one")
        (assert-equal #:expect 3
          #:got (complexity-score (complexity-of inner))
          #:name "and it is measured on its own")))

    (test "the name is the one the form writes, and the line is where it starts"
      (let ((function (measure-first "\n\n(define (make-widget n) n)\n"))
            (variable (measure-first "\n(define default-size 3)\n")))
        (assert-equal #:expect "make-widget" #:got (complexity-name function))
        (assert-equal #:expect 3 #:got (complexity-line function))
        (assert-equal #:expect "default-size" #:got (complexity-name variable))))

    (test "the length is the number of lines the definition covers"
      (let ((measure (measure-first "(define (f x)\n  (let ((y x))\n    y))\n")))
        (assert-equal #:expect 3 #:got (complexity-lines measure))))

    (test "a Racket definition is measured the same way"
      (let ((measure (measure-first "#lang racket/base\n(define (f x)\n  (if (a) 1 2))\n"
                                    #:dialect 'racket)))
        (assert-equal #:expect 1 #:got (complexity-score measure))
        (assert-equal #:expect 2 #:got (complexity-cycles measure))))))
