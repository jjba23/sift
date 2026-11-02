;; sift --- checker: ecase-otherwise
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `ecase' and `etypecase' exist so that the implementation can complain when
;; the clauses do not cover every case.  A default clause is the one thing that
;; turns that complaint off.

(define-module (tests checkers ecase-otherwise)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: ecase-otherwise"

    (test "reports a t clause in ecase"
      (assert-warnings #:source "(ecase x (a 1) (t 2))\n"
                       #:checker 'ecase-otherwise
                       #:dialect 'cl
                       #:expect '((1 16))))

    (test "reports an otherwise clause in etypecase"
      (assert-warnings #:source "(etypecase x (integer 1) (otherwise 2))\n"
                       #:checker 'ecase-otherwise
                       #:dialect 'cl
                       #:expect '((1 26))))

    (test "accepts an ecase that covers its cases"
      (assert-warnings #:source "(ecase x (a 1) (b 2))\n"
                       #:checker 'ecase-otherwise
                       #:dialect 'cl
                       #:expect '()))

    (test "accepts a default clause in case, which is what case is for"
      (assert-warnings #:source "(case x (a 1) (t 2))\n"
                       #:checker 'ecase-otherwise
                       #:dialect 'cl
                       #:expect '()))))
