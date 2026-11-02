;; sift --- checker: append-nothing
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The empty list is the identity of `append', so an argument that is one adds
;; nothing to the result.  What is left is the other list, which is also what
;; the call was copying anyway.

(define-module (tests checkers append-nothing)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: append-nothing"

    (test "reports an append of the empty list"
      (assert-warnings #:source "(append xs '())\n"
                       #:checker 'append-nothing
                       #:expect '((1 1))))

    (test "the fix is the other list"
      (assert-fix #:source "(append xs '())\n"
                  #:checker 'append-nothing
                  #:expected "xs\n"))

    (test "the empty list may come first"
      (assert-fix #:source "(append '() xs)\n"
                  #:checker 'append-nothing
                  #:expected "xs\n"))

    (test "the empty list may sit between two others"
      (assert-fix #:source "(append '(1) '() '(2))\n"
                  #:checker 'append-nothing
                  #:expected "(append '(1) '(2))\n"))

    (test "the fixed files have nothing left to report"
      (for-each
       (lambda (source)
         (assert-fixes-to-nothing #:source source #:checker 'append-nothing
                                  #:name (format #f "~s is clean after its fix" source)))
       '("(append xs '())\n" "(append '() xs)\n" "(append '(1) '() '(2))\n")))

    (test "accepts an append with no empty list"
      (assert-warnings #:source "(append xs ys)\n"
                       #:checker 'append-nothing
                       #:expect '()))))
