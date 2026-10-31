;; sift --- checker: provide-missing
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; An Emacs Lisp file that defines a package and never provides it cannot be
;; required, so `(require 'feature)` fails for anyone who installs it.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers provide-missing)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: provide-missing"

    (test "reports a file that defines but never provides"
      (assert-warning-count #:source "(defun f ()\n  \"Do it.\"\n  nil)\n"
        #:dialect 'elisp #:checker 'provide-missing
        #:count 1))

    (test "accepts a file that provides its feature"
      (assert-warning-count
         #:source "(defun f ()\n  \"Do it.\"\n  nil)\n(provide 'f)\n"
         #:dialect 'elisp #:checker 'provide-missing #:count 0))

    (test "accepts a file that defines nothing"
      (assert-warning-count #:source ";; just a comment\n" #:dialect 'elisp
        #:checker 'provide-missing #:count 0))))
