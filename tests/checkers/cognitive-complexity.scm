;; sift -- checker: cognitive-complexity
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Nesting-depth counts how deep the conditionals go; this rule counts what a
;; reader pays to follow them.  A branch costs one, and a branch inside another
;; costs one more, so two functions with the same number of `if`s are not the
;; same reading: the flat one is a list of cases, the nested one is a tree.
;;
;; The budget is a setting, because a program that spawns processes for a
;; living may nest more than one that formats a buffer.

(define-module (tests checkers cognitive-complexity)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

;; 1 + 2 + 3 + 4: four branches, each one deeper than the last
(define nested-four
  "(define (f x)\n  (if (a)\n    (if (b)\n      (if (c)\n        (if (d) 1 2)))))\n")

;; three branches side by side: three points, not ten
(define flat-three
  "(define (f x)\n  (if (a) 1 2)\n  (if (b) 1 2)\n  (if (c) 1 2))\n")

(define (spec)
  (suite "checker: cognitive-complexity"

    (test "reports a definition over the budget"
      (assert-warnings #:source nested-four #:checker 'cognitive-complexity
                       #:options '((max-cognitive-complexity . 5))
                       #:expect '((1 1))))

    (test "the budget is configuration"
      (assert-warnings #:source nested-four #:checker 'cognitive-complexity
                       #:options '((max-cognitive-complexity . 15))
                       #:expect '()))

    (test "branches side by side cost less than the same branches nested"
      (assert-warnings #:source flat-three #:checker 'cognitive-complexity
                       #:options '((max-cognitive-complexity . 5))
                       #:expect '()))

    (test "a quoted datum does not push a definition over the budget"
      (assert-warnings
       #:source "(define pattern '(if a (if b (if c 1 2))))\n"
       #:checker 'cognitive-complexity
       #:options '((max-cognitive-complexity . 1)) #:expect '()))

    (test "a nested definition is reported on its own line, not its parent's"
      (assert-warnings
       #:source "(define (outer)\n  (define (inner)\n    (if (a) (if (b) (if (c) (if (d) 1 2)))))\n  (if e 1 2))\n"
       #:checker 'cognitive-complexity
       #:options '((max-cognitive-complexity . 5))
       #:expect '((2 3))))

    (test "a Racket definition is measured the same way"
      (assert-warnings #:source (string-append "#lang racket/base\n" nested-four)
                       #:dialect 'racket
                       #:checker 'cognitive-complexity
                       #:options '((max-cognitive-complexity . 5))
                       #:expect '((2 1))))))
