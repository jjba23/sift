;; sift --- checker: comment-semicolons
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Riastradh's rule: four semicolons for a heading, three for a top-level
;; comment, two for a comment inside a form, one for a comment at the end of a
;; line.  The number is what tells a reader where the comment belongs before
;; the sentence is read, so a comment that is off by a level is a comment that
;; points at the wrong thing.
;;
;; A margin comment is never reported: one semicolon is a gesture there, not a
;; level, and `;Zibblefrotz' at the end of a line is how that position is
;; written.

(define-module (tests checkers comment-semicolons)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: comment-semicolons"

    (test "reports a top-level comment with two semicolons"
      (assert-warnings #:source ";; not quite top level\n(define x 1)\n"
                       #:checker 'comment-semicolons #:expect '((1 1))))

    (test "accepts a top-level comment with three semicolons"
      (assert-warnings #:source ";;; a section of the program\n(define x 1)\n"
                       #:checker 'comment-semicolons #:expect '()))

    (test "accepts a heading, which has four"
      (assert-warnings #:source ";;;; Frob Grovel\n(define x 1)\n"
                       #:checker 'comment-semicolons #:expect '()))

    (test "reports a heading inside a form, where two is the level"
      (assert-warnings #:source "(define (f x)\n  ;;; a heading in a body\n  x)\n"
                       #:checker 'comment-semicolons #:expect '((2 3))))

    (test "reports a one-semicolon comment inside a form"
      (assert-warnings #:source "(define (f x)\n  ; one is for the margin\n  x)\n"
                       #:checker 'comment-semicolons #:expect '((2 3))))

    (test "accepts two semicolons inside a form"
      (assert-warnings #:source "(define (f x)\n  ;; about x\n  x)\n"
                       #:checker 'comment-semicolons #:expect '()))

    (test "reports a comment with no space after its semicolons"
      (assert-warnings #:source ";;;tight\n(define x 1)\n"
                       #:checker 'comment-semicolons #:expect '((1 1))))

    (test "accepts a margin comment with one semicolon"
      (assert-warnings #:source "(a b) ; the second thing\n"
                       #:checker 'comment-semicolons #:expect '()))

    (test "accepts a margin comment written without a space"
      (assert-warnings #:source "(a b) ;Zibblefrotz.\n"
                       #:checker 'comment-semicolons #:expect '()))

    (test "says nothing about a block comment, which has no semicolons"
      (assert-warnings #:source "#| a block comment |#\n(define x 1)\n"
                       #:checker 'comment-semicolons #:expect '()))

    ;; The fix is the mechanical half of the rule: the level a comment takes is
    ;; decided by where it sits, which the reader knows.  What the comment says,
    ;; and whether it sits in the right place at all, is still the author's.
    (test "the fix raises a top-level comment to three semicolons"
      (assert-fix #:source ";; about the program\n(define x 1)\n"
                  #:checker 'comment-semicolons
                  #:expected ";;; about the program\n(define x 1)\n"))

    (test "the fix lowers a comment inside a form to two semicolons"
      (assert-fix #:source "(define (f x)\n  ;;;; a heading in a body\n  x)\n"
                  #:checker 'comment-semicolons
                  #:expected "(define (f x)\n  ;; a heading in a body\n  x)\n"))

    (test "the fix adds the space after the semicolons and nothing else"
      (assert-fix #:source ";;;tight\n(define x 1)\n"
                  #:checker 'comment-semicolons
                  #:expected ";;; tight\n(define x 1)\n"))

    (test "the fix leaves a heading at four semicolons"
      (assert-fix #:source ";;;; Frob Grovel\n(define x 1)\n"
                  #:checker 'comment-semicolons
                  #:expected ";;;; Frob Grovel\n(define x 1)\n"))

    (test "the fix leaves a margin comment alone"
      (assert-fix #:source "(a b) ;Zibblefrotz.\n"
                  #:checker 'comment-semicolons
                  #:expected "(a b) ;Zibblefrotz.\n"))

    (test "the fix leaves the sentence of the comment exactly as it was"
      ;; two spaces after the semicolons become one, and nothing else moves
      (assert-fix #:source ";;  spaced  out  \n(define x 1)\n"
                  #:checker 'comment-semicolons
                  #:expected ";;; spaced  out  \n(define x 1)\n"))

    (test "the fixed buffer is clean, so a second run has nothing to do"
      (assert-fixes-to-nothing
       #:source ";; one\n(define (f x)\n  ;;;; two\n  ;;tight\n  x)\n;; four\n"
       #:checker 'comment-semicolons))

    (test "one pass fixes every comment in the buffer"
      (assert-fix #:source (string-append ";; first\n"
                                          "(define (f x)\n"
                                          "  ;;; second\n"
                                          "  x)\n"
                                          ";;;third\n")
                  #:checker 'comment-semicolons
                  #:expected (string-append ";;; first\n"
                                            "(define (f x)\n"
                                            "  ;; second\n"
                                            "  x)\n"
                                            ";;; third\n")))))
