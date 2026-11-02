;; sift --- checker: package-description-literal
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; xgettext extracts a description only when it can read it as one string, so
;; the manual asks for a literal and rules out `string-append' and `format'.

(define-module (tests checkers package-description-literal)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: package-description-literal"

    (test "reports a description built with `string-append`"
      (assert-warnings #:source "(package (description (string-append \"Frob \" \"files.\")))\n"
                       #:checker 'package-description-literal
                       #:expect '((1 23))))

    (test "reports a description built with `format`"
      (assert-warnings #:source "(package (description (format #f \"~a\" name)))\n"
                       #:checker 'package-description-literal
                       #:expect '((1 23))))

    (test "accepts a literal description"
      (assert-warnings #:source "(package (description \"Frob files.\"))\n"
                       #:checker 'package-description-literal
                       #:expect '()))

    (test "accepts a package with no description field"
      (assert-warnings #:source "(package (name \"x\"))\n"
                       #:checker 'package-description-literal
                       #:expect '()))))
