;; sift -- checker: unused-import
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; An import nothing uses is a claim the file stopped keeping: the reader looks
;; up the library, finds the name, and the name is never mentioned again.  What
;; sift can know is bounded on purpose -- the names an `only' clause writes
;; down, the pairs a `rename' writes down, and a library whose own `(export
;; ...)' it can read -- so the tests state both halves: the import that is
;; reported, and the plain `(import (scheme base))' that is not.

(define-module (tests checkers unused-import)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: unused-import"

    (test "reports the name of an `only` clause nothing uses"
      (assert-warnings
       #:source "(import (only (srfi 1) fold car))\n(display (fold + 0 (list 1 2)))\n"
       #:checker 'unused-import #:expect '((1 29))))

    (test "the fix deletes the name and the space before it"
      (assert-fix
       #:source "(import (only (srfi 1) fold car))\n(display (fold + 0 (list 1 2)))\n"
       #:checker 'unused-import
       #:expected "(import (only (srfi 1) fold))\n(display (fold + 0 (list 1 2)))\n"))

    (test "an `only` clause with nothing used is one finding, on the form"
      (assert-warnings
       #:source "(import (only (srfi 1) car cdr))\n(display 1)\n"
       #:checker 'unused-import #:expect '((1 1))))

    (test "the fix removes the whole import when it is the only thing on its line"
      (assert-fix
       #:source "(import (only (srfi 1) car cdr))\n(display 1)\n"
       #:checker 'unused-import
       #:expected "(display 1)\n"))

    (test "one unused spec of two is deleted, and the form stays"
      (assert-fix
       #:source "(import (scheme base) (only (srfi 1) car))\n(display 1)\n"
       #:checker 'unused-import
       #:expected "(import (scheme base))\n(display 1)\n"))

    (test "reports a renamed import nothing uses, by its new name"
      (assert-fix
       #:source "(import (rename (srfi 1) (fold foldl) (car first)))\n(display (foldl + 0 (list 1)))\n"
       #:checker 'unused-import
       #:expected "(import (rename (srfi 1) (fold foldl)))\n(display (foldl + 0 (list 1)))\n"))

    (test "a name used only inside a quote does not count as used"
      (assert-warnings
       #:source "(import (only (srfi 1) car))\n(display '(car))\n"
       #:checker 'unused-import #:expect '((1 1))))

    (test "a plain import of a library sift cannot read is left alone"
      (assert-warnings
       #:source "(import (scheme base) (srfi 1))\n(display 1)\n"
       #:checker 'unused-import #:expect '()))

    (test "an import used inside a library body is read"
      (assert-warnings
       #:source "(define-library (my lib)\n  (export run)\n  (import (scheme base) (only (srfi 1) car))\n  (define (run) 1))\n"
       #:checker 'unused-import #:expect '((3 25))))

    (test "every name in an `only` clause used is silent"
      (assert-warnings
       #:source "(import (only (srfi 1) fold))\n(display (fold + 0 (list 1)))\n"
       #:checker 'unused-import #:expect '()))))
