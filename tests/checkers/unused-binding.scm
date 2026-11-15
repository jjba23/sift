;; sift -- checker: unused-binding
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A `let` binding nobody reads is either a leftover from an edit or a name the
;; author meant to use.  It has no fix, because the two answers -- delete the
;; binding, or use it -- are a decision and not an edit, and `_' in front of the
;; name is how a reader says the value is deliberately dropped.
;;
;; The rule reads the binding list rather than the body's text, so it works the
;; same way in all four dialects, whose `let` spellings differ only in brackets.

(define-module (tests checkers unused-binding)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: unused-binding"

    (test "reports a binding the body never uses"
      (assert-warnings #:source "(let ((x 1) (y 2)) x)\n"
                       #:checker 'unused-binding #:expect '((1 14))))

    (test "accepts a let whose bindings are all used"
      (assert-warnings #:source "(let ((x 1) (y 2)) (+ x y))\n"
                       #:checker 'unused-binding #:expect '()))

    (test "accepts a name written with a leading underscore"
      ;; `_' is how the dialect says "I know, and I am throwing it away"
      (assert-warnings #:source "(let ((_x 1)) 2)\n"
                       #:checker 'unused-binding #:expect '()))

    (test "reads a let* and reports the binding it finds unused"
      (assert-warnings #:source "(let* ((a 1) (b (+ a 1))) a)\n"
                       #:checker 'unused-binding #:expect '((1 15))))

    (test "accepts a named let, whose bindings the loop itself uses"
      (assert-warnings #:source "(let loop ((i 0)) 1)\n"
                       #:checker 'unused-binding #:expect '()))

    (test "reads a binding inside a definition"
      (assert-warnings #:source "(define (f) (let ((v 1)) 2))\n"
                       #:checker 'unused-binding #:expect '((1 20))))

    (test "reads the dialect's own let"
      ;; the same rule, in the spelling each dialect writes
      (for-each
       (lambda (example)
         (assert-warnings #:source (cadr example)
                          #:dialect (car example)
                          #:checker 'unused-binding
                          #:expect (caddr example)))
       '((elisp "(let ((a 1)) 2)\n" ((1 8)))
         (racket "(let ([a 1]) 2)\n" ((1 8)))
         (cl "(let ((a 1)) 2)\n" ((1 8))))))))
