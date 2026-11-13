;; sift --- checker: wrap-in-let
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A body form that nests five levels deep or lists a dozen elements is a
;; form a reader has to hold in their head all at once.  Binding the part
;; that repeats gives it a name; `let*` when the bindings depend on each
;; other.  The checker suggests, and leaves the naming to the author, so it
;; has no fix.
;;
;; The thresholds are configuration (`max-depth` and `max-elements`), because
;; a data-heavy script and a compiler are read at different densities.

(define-module (tests checkers wrap-in-let)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define deep-source
  "(define (f x)\n  (list (car (cdr (assq 'a (cdr x))))))\n")

(define wide-source
  (string-append "(define (f)\n  (list " (string-join (map number->string (iota 13)) " ") "))\n"))

(define wide-cond-source
  (string-append "(defun f (x)\n  (cond\n"
                 (string-join
                  (map (lambda (n)
                         (string-append "        ((= x " (number->string n) ") "
                                        (number->string n) ")\n"))
                       (iota 13 1))
                  "")
                 "        (t nil)))\n"))

;; Issue #9, in the shape it was reported.
(define issue-nine-source
  "(defun test (s)\n  (loop for c across s\n        for pointer :of-type fixnum = 0 then (1+ pointer)\n        with res :of-type list = nil\n        do (push c res)))\n")

(define (spec)
  (suite "checker: wrap-in-let"

    (test "reports a body form that nests too deeply"
      (assert-warnings #:source deep-source #:checker 'wrap-in-let
                       #:options '((max-depth . 4)) #:expect '((2 3))))

    (test "reports a body form with too many elements"
      (assert-warnings #:source wide-source #:checker 'wrap-in-let
                       #:options '((max-elements . 12)) #:expect '((2 3))))

    (test "accepts a body form that is already split up"
      (assert-warnings #:source "(define (f x)\n  (let ((y (cdr x)))\n    (list (car y))))\n"
                       #:checker 'wrap-in-let #:expect '()))

    ;; Issue #9: a `loop' is made of clauses, and its elements are those
    ;; clauses rather than one expression the author should bind in a `let'.
    ;; Every check about a form's elements has nothing to say about it.
    (test "a loop is clauses, not a body form of twenty-one elements"
      (assert-warnings #:source issue-nine-source #:dialect 'elisp
                       #:checker 'wrap-in-let #:expect '()))

    (test "a cond of many clauses is clauses too"
      (assert-warnings #:source wide-cond-source #:dialect 'elisp
                       #:checker 'wrap-in-let
                       #:options '((max-elements . 12)) #:expect '()))

    (test "the depth limit is configuration"
      (assert-warnings #:source deep-source #:checker 'wrap-in-let
                       #:options '((max-depth . 8)) #:expect '()))

    (test "the element limit is configuration"
      (assert-warnings #:source wide-source #:checker 'wrap-in-let
                       #:options '((max-elements . 40)) #:expect '()))

    (test "there is no automatic fix"
      (assert-equal #:expect #f
        #:got (lint-warning-fix
               (car (warnings-of deep-source #:checker 'wrap-in-let
                                 #:options '((max-depth . 4)))))))))
