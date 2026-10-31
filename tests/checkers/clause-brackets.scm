;; sift --- checker: clause-brackets
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Guile's own sources, and the Guix code base, write a `cond' clause as
;; `[(= x 1) ...]': the bracket is the visual boundary of the clause, and the
;; reader sees where the clause ends without counting parentheses.
;;
;; The rule follows the `brackets' setting, so a project that writes `(' and
;; means it gets a rule that says so instead of silence.  The fix rewrites
;; the two delimiters and nothing else.

(define-module (tests checkers clause-brackets)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (sift format)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: clause-brackets"

    (test "reports a cond clause written with parentheses"
      (assert-warnings #:source "(cond ((= x 1) 'one)\n      (else 'other))\n"
        #:checker 'clause-brackets #:expect '((1 7) (2 7))))

    (test "accepts a cond clause written with square brackets"
      (assert-warnings #:source "(cond [(= x 1) 'one]\n      [else 'other])\n"
        #:checker 'clause-brackets #:expect '()))

    (test "reports a case clause written with parentheses"
      (assert-warnings #:source "(case x ((1) 'one)\n  ((2) 'two))\n"
        #:checker 'clause-brackets #:expect '((1 9) (2 3))))

    (test "follows a project that writes parentheses"
      (assert-warnings #:source "(cond [(= x 1) 'one])\n" #:checker 'clause-brackets
        #:options '((brackets . parens)) #:expect '((1 7))))

    (test "says nothing when the project preserves what was written"
      (assert-warnings #:source "(cond ((= x 1) 'one))\n" #:checker 'clause-brackets
        #:options '((brackets . preserve)) #:expect '()))

    (test "the fix rewrites the delimiters"
      (assert-fix #:source "(cond ((= x 1) 'one)\n      (else 'other))\n"
        #:checker 'clause-brackets
        #:expected "(cond [(= x 1) 'one]\n      [else 'other])\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing
         #:source "(cond ((= x 1) 'one)\n      (else 'other))\n"
         #:checker 'clause-brackets))

    (test "the default for Scheme is square"
      (assert-equal #:expect 'square #:got (default-brackets 'scheme)))

    (test "the default for the other dialects is to preserve"
      (assert-equal #:expect 'preserve #:got (default-brackets 'elisp))
      (assert-equal #:expect 'preserve #:got (default-brackets 'cl)))))
