;; sift --- checker: sha256-base32
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A hash is written `(base32 "...")', and 32 bytes in base32 is 52 lower-case
;; characters.  A shorter string cannot be the hash the download has, and a
;; `sha256' of a bare string is not a hash Guix can compare at all.

(define-module (tests checkers sha256-base32)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (base32-source hash)
  (string-append "(origin (sha256 (base32 \"" hash "\")))\n"))

(define (spec)
  (suite "checker: sha256-base32"

    (test "reports a hash that is not written in base32"
      (assert-warnings #:source "(origin (sha256 \"abc\"))\n"
                       #:checker 'sha256-base32
                       #:expect '((1 17))))

    (test "reports a base32 string of the wrong length"
      (assert-warnings #:source "(origin (sha256 (base32 \"abc\")))\n"
                       #:checker 'sha256-base32
                       #:expect '((1 17))))

    (test "accepts 52 lower-case characters"
      (assert-warnings #:source (base32-source (make-string 52 #\a))
                       #:checker 'sha256-base32
                       #:expect '()))

    (test "reports a hash with an upper-case character"
      (assert-warnings #:source (base32-source (string-append (make-string 51 #\a) "A"))
                       #:checker 'sha256-base32
                       #:expect '((1 17))))

    (test "leaves a `sha256` outside an origin alone"
      (assert-warnings #:source "(sha256 (base32 \"abc\"))\n"
                       #:checker 'sha256-base32
                       #:expect '()))))
