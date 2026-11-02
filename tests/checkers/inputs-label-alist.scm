;; sift --- checker: inputs-label-alist
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `(inputs `(("gawk" ,gawk)))' names every package twice: once by the label a
;; build phase has to look up, and once by the variable it already has.  An
;; input list is a list of packages, and gexps made the labels unnecessary.

(define-module (tests checkers inputs-label-alist)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: inputs-label-alist"

    (test "reports a labelled input list"
      (assert-warnings #:source "(package (inputs `((\"gawk\" ,gawk))))\n"
                       #:checker 'inputs-label-alist
                       #:expect '((1 10))))

    (test "the fix writes the list of packages"
      (assert-fix #:source "(package (inputs `((\"gawk\" ,gawk))))\n"
                  #:checker 'inputs-label-alist
                  #:expected "(package (inputs (list gawk)))\n")
      (assert-fixes-to-nothing #:source "(package (inputs `((\"gawk\" ,gawk))))\n"
                               #:checker 'inputs-label-alist))

    (test "reports the other two input lists as well"
      (assert-warnings #:source "(package (native-inputs `((\"pkg-config\" ,pkg-config))))\n"
                       #:checker 'inputs-label-alist
                       #:expect '((1 10)))
      (assert-warnings #:source "(package (propagated-inputs `((\"gnutls\" ,gnutls))))\n"
                       #:checker 'inputs-label-alist
                       #:expect '((1 10))))

    (test "accepts a list of packages"
      (assert-warnings #:source "(package (inputs (list gawk)))\n"
                       #:checker 'inputs-label-alist
                       #:expect '()))

    (test "leaves an alist outside a package alone"
      (assert-warnings #:source "(inputs `((\"gawk\" ,gawk)))\n"
                       #:checker 'inputs-label-alist
                       #:expect '()))))
