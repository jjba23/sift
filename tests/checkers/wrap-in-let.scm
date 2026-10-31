;; sift --- checker: wrap-in-let
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

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

(define (spec)
  (suite "checker: wrap-in-let"

    (test "reports a body form that nests too deeply"
      (assert-warnings #:source deep-source #:checker 'wrap-in-let
        #:expect '((2 3))))

    (test "reports a body form with too many elements"
      (assert-warnings #:source wide-source #:checker 'wrap-in-let
        #:expect '((2 3))))

    (test "accepts a body form that is already split up"
      (assert-warnings #:source "(define (f x)\n  (let ((y (cdr x)))\n    (list (car y))))\n"
        #:checker 'wrap-in-let #:expect '()))

    (test "the depth limit is configuration"
      (assert-warnings #:source deep-source #:checker 'wrap-in-let
        #:options '((max-depth . 8)) #:expect '()))

    (test "the element limit is configuration"
      (assert-warnings #:source wide-source #:checker 'wrap-in-let
        #:options '((max-elements . 40)) #:expect '()))

    (test "there is no automatic fix"
      (assert-equal #:expect #f
        #:got (lint-warning-fix (car (warnings-of deep-source
                                       #:checker 'wrap-in-let)))))))
