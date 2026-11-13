;; sift --- checker: duplicate-keyword
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A keyword argument given twice in one call silently drops one of the two
;; values, so the rule names the second one.  What it must not report is a
;; form whose elements are clauses: every keyword of a `loop' starts a clause,
;; so `:of-type' twice declares the type of two different variables, and two
;; `:do' clauses both run.

(define-module (tests checkers duplicate-keyword)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: duplicate-keyword"

    (test "reports a keyword given twice in one call"
      (assert-warnings #:source "(make-window :width 10 :width 20)\n"
                       #:checker 'duplicate-keyword
                       #:expect '((1 24))))

    (test "says nothing when every keyword is given once"
      (assert-warnings #:source "(make-window :width 10 :height 20)\n"
                       #:checker 'duplicate-keyword
                       #:expect '()))

    ;; Issue #9, in the shape it was reported: two `:of-type' in one `loop'
    ;; belong to two variables, and nothing is being discarded.
    (test "a variable's type in a loop is not a duplicate keyword"
      (assert-warnings
       #:source "(defun test (s)\n  (loop for c across s\n        for pointer :of-type fixnum = 0 then (1+ pointer)\n        with res :of-type list = nil\n        do (push c res)))\n"
       #:dialect 'elisp #:checker 'duplicate-keyword #:expect '()))

    (test "two do clauses in a loop are two clauses, not a duplicate"
      (assert-warnings #:source "(loop for x in xs do (f x) do (g x))\n"
                       #:dialect 'cl #:checker 'duplicate-keyword
                       #:expect '()))

    ;; The exemption is the form, not the file: a call that repeats a keyword
    ;; inside a loop clause is still a call that discards a value.
    (test "a duplicate inside a loop's clause is still a duplicate"
      (assert-warnings
       #:source "(loop for x in xs do (make-window :width 1 :width 2))\n"
       #:dialect 'cl #:checker 'duplicate-keyword #:expect '((1 44))))))
