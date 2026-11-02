;; sift --- checker: defpackage-use
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `(:use ...)' puts every external symbol of a package into the reader's
;; scope, so a name that arrives from it cannot be traced to the line that chose
;; it.  The Google Common Lisp style guide asks for `:import-from' instead, and
;; exempts `:cl' itself.

(define-module (tests checkers defpackage-use)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: defpackage-use"

    (test "reports a :use of a library other than :cl"
      (assert-warnings #:source "(defpackage :demo (:use :cl :alexandria))\n"
                       #:checker 'defpackage-use
                       #:dialect 'cl
                       #:expect '((1 29))))

    (test "reports each library it finds"
      (assert-warning-count
       #:source "(defpackage :demo (:use :cl :alexandria :cl-ppcre))\n"
       #:checker 'defpackage-use #:dialect 'cl #:count 2))

    (test "accepts a package that only uses :cl"
      (assert-warnings #:source "(defpackage :demo (:use :cl))\n"
                       #:checker 'defpackage-use
                       #:dialect 'cl
                       #:expect '()))

    (test "accepts :import-from"
      (assert-warnings
       #:source "(defpackage :demo (:use :cl) (:import-from :alexandria #:with-gensyms))\n"
       #:checker 'defpackage-use
       #:dialect 'cl
       #:expect '()))))
