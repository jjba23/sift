;; sift --- checker: package-prefix
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Google's Common Lisp style guide asks production code to name the package a
;; symbol comes from rather than relying on the reader's current one.  The
;; export list of the file's own `defpackage` is what makes the check possible.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers package-prefix)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: package-prefix"

    (test "reports an exported symbol used without a prefix"
      (assert-warnings
         #:source "(defpackage :app (:export #:run))\n(defun run () \"Run.\" nil)\n(run)\n"
         #:dialect 'cl #:checker 'package-prefix #:expect '((3 2))))

    (test "accepts a prefixed use"
      (assert-warnings
         #:source "(defpackage :app (:export #:run))\n(app:run)\n"
         #:dialect 'cl #:checker 'package-prefix #:expect '()))

    (test "accepts a file with no export list"
      (assert-warnings #:source "(defun run () \"Run.\" nil)\n(run)\n"
        #:dialect 'cl #:checker 'package-prefix #:expect '()))))
