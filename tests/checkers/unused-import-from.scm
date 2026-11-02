;; sift --- checker: unused-import-from
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A package definition imports a name to use it.  One that is never used is a
;; name the reader has to look up for nothing, and `:import-from` makes it
;; invisible: the symbol is in the package either way.

(define-module (tests checkers unused-import-from)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: unused-import-from"

    (test "reports an imported name the file never uses"
      (assert-warning-count
       #:source "(defpackage :demo (:use :cl) (:import-from :alexandria #:curry))\n(in-package :demo)\n(print 1)\n"
       #:checker 'unused-import-from
       #:dialect 'cl
       #:count 1))

    (test "accepts an imported name the file uses"
      (assert-warnings
       #:source "(defpackage :demo (:use :cl) (:import-from :alexandria #:curry))\n(in-package :demo)\n(curry #'list)\n"
       #:checker 'unused-import-from
       #:dialect 'cl
       #:expect '()))

    (test "reads every name of the clause"
      (assert-warning-count
       #:source "(defpackage :demo (:use :cl) (:import-from :alexandria #:curry #:rcurry))\n(curry #'list)\n"
       #:checker 'unused-import-from
       #:dialect 'cl
       #:count 1))

    (test "a package with no import clause is fine"
      (assert-warnings #:source "(defpackage :demo (:use :cl))\n(in-package :demo)\n"
                       #:checker 'unused-import-from
                       #:dialect 'cl
                       #:expect '()))))
