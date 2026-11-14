;; sift -- checker: cyclomatic-complexity
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; One plus the number of decisions is the number of paths a test has to
;; cover, which is why the line is drawn by a budget rather than at zero: a
;; procedure with no branches has one path, and every `if` is one more.
;;
;; A `cond' is one decision here, not one per clause, because the rule speaks
;; the same numbers as `maak complexity'; the clauses are read as the cases of
;; one choice rather than as a chain of them.

(define-module (tests checkers cyclomatic-complexity)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define three-branches
  "(define (f x)\n  (if (a) 1 2)\n  (if (b) 1 2)\n  (if (c) 1 2))\n")

(define (spec)
  (suite "checker: cyclomatic-complexity"

    (test "reports a definition with more paths than the budget"
      ;; three branches and the way in: four paths
      (assert-warnings #:source three-branches #:checker 'cyclomatic-complexity
                       #:options '((max-cyclomatic-complexity . 3))
                       #:expect '((1 1))))

    (test "the budget is configuration"
      (assert-warnings #:source three-branches #:checker 'cyclomatic-complexity
                       #:options '((max-cyclomatic-complexity . 4))
                       #:expect '()))

    (test "a definition with no branch has one path"
      (assert-warnings #:source "(define (f x) (g x))\n"
                       #:checker 'cyclomatic-complexity
                       #:options '((max-cyclomatic-complexity . 1)) #:expect '()))

    (test "a `cond` is one decision, not one per clause"
      ;; three clauses and the way in: two paths, not four
      (assert-warnings
       #:source "(define (f x)\n  (cond [(a) 1]\n        [(b) 2]\n        [else 3]))\n"
       #:checker 'cyclomatic-complexity
       #:options '((max-cyclomatic-complexity . 2)) #:expect '())
      (assert-warnings
       #:source "(define (f x)\n  (cond [(a) 1]\n        [(b) 2]\n        [else 3]))\n"
       #:checker 'cyclomatic-complexity
       #:options '((max-cyclomatic-complexity . 1)) #:expect '((1 1))))))
