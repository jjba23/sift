;; sift --- checker: package-synopsis
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The manual's three sentences about synopses, turned into three findings:
;; start with a capital, do not end with a period, and do not open with an
;; article, "which usually does not bring anything".

(define-module (tests checkers package-synopsis)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: package-synopsis"

    (test "reports a synopsis that opens with an article and ends with a period"
      (assert-warnings #:source "(package (name \"x\") (synopsis \"A file frobber.\"))\n"
                       #:checker 'package-synopsis
                       #:expect '((1 31) (1 31))))

    (test "reports a synopsis that starts in lower case"
      (assert-warnings #:source "(package (synopsis \"lower case\"))\n"
                       #:checker 'package-synopsis
                       #:expect '((1 20))))

    (test "accepts a synopsis as the manual asks for it"
      (assert-warnings #:source "(package (synopsis \"File frobber\"))\n"
                       #:checker 'package-synopsis
                       #:expect '()))

    (test "leaves a `synopsis` outside a package alone"
      (assert-warnings #:source "(synopsis \"a thing.\")\n"
                       #:checker 'package-synopsis
                       #:expect '()))))
