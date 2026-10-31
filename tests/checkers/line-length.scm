;; sift --- checker: line-length
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The soft limit is a warning, never a reason to reflow: breaking a form is
;; the author's decision.  Emacs Lisp gets 80 columns and the other two 100.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers line-length)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: line-length"

    (test "reports a line over the Scheme limit"
      (assert-warnings #:source (string-append "(" (make-string 120 #\x) ")\n")
        #:checker 'line-length #:expect '((1 101))))

    (test "reports a line over the Emacs Lisp limit, which is 80"
      (assert-warnings #:source (string-append "(" (make-string 90 #\x) ")\n")
        #:checker 'line-length #:dialect 'elisp
        #:expect '((1 81))))

    (test "accepts a short line"
      (assert-warnings #:source "(define x 1)\n" #:checker 'line-length
        #:expect '()))

    (test "the limit can be configured"
      (assert-warnings #:source "(define x 1)\n" #:checker 'line-length
        #:options '((line-length . 5))
        #:expect '((1 6))))))
