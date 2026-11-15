;; sift -- checker: duplicate-binding
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; One name, two values, and the first of them unreachable: in a `let', the
;; later binding wins and the earlier one is dead text.  The rule has no fix
;; because which of the two the author meant is not a question a formatter can
;; answer.
;;
;; A `let` inside a `let` is not a duplicate.  Shadowing is how Lisp says "the
;; same name, a narrower meaning", and a rule that reported it would report most
;; of the code ever written.

(define-module (tests checkers duplicate-binding)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: duplicate-binding"

    (test "reports the same name bound twice in one let"
      (assert-warnings #:source "(let ((x 1) (x 2)) x)\n"
                       #:checker 'duplicate-binding #:expect '((1 13))))

    (test "accepts a let whose names are different"
      (assert-warnings #:source "(let ((x 1) (y 2)) (+ x y))\n"
                       #:checker 'duplicate-binding #:expect '()))

    (test "accepts a shadowing let, which is a narrower name and not a mistake"
      (assert-warnings #:source "(let ((x 1)) (let ((x 2)) x))\n"
                       #:checker 'duplicate-binding #:expect '()))

    (test "reads a let*"
      (assert-warnings #:source "(let* ((x 1) (x 2)) x)\n"
                       #:checker 'duplicate-binding #:expect '((1 14))))

    (test "reads the dialect's own let"
      (for-each
       (lambda (example)
         (assert-warnings #:source (cadr example)
                          #:dialect (car example)
                          #:checker 'duplicate-binding
                          #:expect (caddr example)))
       '((elisp "(let ((a 1) (a 2)) a)\n" ((1 13)))
         ;; Racket's square brackets are the same binding list
         (racket "(let ([x 1] [x 2]) x)\n" ((1 13)))
         (cl "(let ((a 1) (a 2)) a)\n" ((1 13))))))))
