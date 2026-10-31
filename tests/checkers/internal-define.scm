;; sift --- checker: internal-define
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Scheme lets a body start with definitions, but a `define` in the middle of
;; an expression defines in the wrong scope.  A `let` in between is the nested
;; scope the author meant, so it is not reported.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers internal-define)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: internal-define"

    (test "reports a define that comes after the first expression"
      (assert-warnings #:source "(define (f)\n  (display 1)\n  (define y 1)\n  y)\n"
        #:checker 'internal-define #:expect '((3 3))))

    (test "reports a define inside a lambda, after an expression"
      (assert-warnings #:source "(lambda ()\n  (display 1)\n  (define y 1)\n  y)\n"
        #:checker 'internal-define #:expect '((3 3))))

    (test "accepts definitions at the head of a body"
      ;; Scheme allows any number of definitions there, and a docstring may
      ;; come first: this is the shape of a module-level procedure
      (assert-warnings
         #:source "(define (f)\n  \"Do it.\"\n  (define y 1)\n  (define z 2)\n  (+ y z))\n"
         #:checker 'internal-define #:expect '()))

    (test "accepts a definition under a let"
      (assert-warnings #:source "(define (f)\n  (let ((y 1)) y))\n"
        #:checker 'internal-define #:expect '()))

    (test "accepts top-level definitions"
      (assert-warnings #:source "(define x 1)\n(define (f) x)\n"
        #:checker 'internal-define #:expect '()))))
