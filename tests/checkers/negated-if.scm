;; sift --- checker: negated-if
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `(if (not x) a b)' is a sentence with two negations in it: the test says
;; what is *not* the case, and the reader has to flip it to follow the
;; branches.  `unless' says it once, and so does `(when (not x) ...)'.
;;
;; The fix is safe because the syntactic shape does not change: the same
;; test is evaluated, the same branches run.

(define-module (tests checkers negated-if)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: negated-if"

    (test "reports an if with a negated test"
      (assert-warnings #:source "(if (not (ready?)) (wait) (go))\n"
        #:checker 'negated-if #:expect '((1 1))))

    (test "reports an if with a negated test and no else branch"
      (assert-warnings #:source "(if (not (ready?)) (wait))\n"
        #:checker 'negated-if #:expect '((1 1))))

    (test "reports a when with a negated test"
      (assert-warnings #:source "(when (not x) (go))\n"
        #:checker 'negated-if #:expect '((1 1))))

    (test "accepts an unless"
      (assert-warnings #:source "(unless (ready?) (wait))\n"
        #:checker 'negated-if #:expect '()))

    (test "accepts an if whose test is not a negation"
      (assert-warnings #:source "(if (ready?) (go) (wait))\n"
        #:checker 'negated-if #:expect '()))

    (test "the fix rewrites the if as an unless"
      (assert-fix #:source "(if (not (ready?)) (wait) (go))\n"
        #:checker 'negated-if
        #:expected "(unless (ready?) (wait) (go))\n"))

    (test "the fix rewrites a when as an unless"
      (assert-fix #:source "(when (not x) (go))\n" #:checker 'negated-if
        #:expected "(unless x (go))\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "(if (not (ready?)) (wait) (go))\n"
        #:checker 'negated-if))))
