;; sift --- checker: mixed-lambda-list
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A lambda list with `&optional' before `&key' accepts both, and which kind an
;; argument is depends on how many the caller passed: the optional parameters
;; take their arguments first, so the first keyword a caller writes is read as
;; an optional one.

(define-module (tests checkers mixed-lambda-list)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: mixed-lambda-list"

    (test "reports a defun that takes both"
      (assert-warnings #:source "(defun f (x &optional y &key z) (list x y z))\n"
                       #:checker 'mixed-lambda-list
                       #:dialect 'cl
                       #:expect '((1 10))))

    (test "reports a lambda that takes both"
      (assert-warnings #:source "(lambda (a &optional b &key c) (list a b c))\n"
                       #:checker 'mixed-lambda-list
                       #:dialect 'cl
                       #:expect '((1 9))))

    (test "accepts keywords alone"
      (assert-warnings #:source "(defun f (x &key y z) (list x y z))\n"
                       #:checker 'mixed-lambda-list
                       #:dialect 'cl
                       #:expect '()))

    (test "accepts optionals alone"
      (assert-warnings #:source "(defun f (x &optional y) (list x y))\n"
                       #:checker 'mixed-lambda-list
                       #:dialect 'cl
                       #:expect '()))

    (test "accepts a plain lambda list"
      (assert-warnings #:source "(defun f (x y) (list x y))\n"
                       #:checker 'mixed-lambda-list
                       #:dialect 'cl
                       #:expect '()))))
