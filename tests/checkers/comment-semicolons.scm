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
                       #:checker 'comment-semicolons #:expect '()))))
