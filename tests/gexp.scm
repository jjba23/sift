;; sift --- gexps
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A gexp is a reader prefix, not a form: `#~', `#$', `#$@', `#+' and `#+@'.
;; The reader has to keep each of them as a prefix on its datum -- the shape
;; the formatter glues back together and the linter reads a build out of --
;; while `#+feature' stays a Common Lisp conditional, which is the meaning it
;; has in a file that is not Scheme.

(define-module (tests gexp)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift format)
  #:use-module (tests helpers)
  #:export (spec))

(define (formatted source)
  (format-string source #:dialect 'scheme))

(define (prefix token)
  (string->symbol token))

(define (spec)
  (suite "gexps"

    (test "`#~` is a prefix on the datum it makes"
      (assert-equal #:expect (list (list (prefix "#~") '(mkdir output)))
        #:got (read-sexps "#~(mkdir output)\n")))

    (test "`#$` is a prefix, not part of the atom"
      (assert-equal #:expect (list (list 'list (list (prefix "#$") 'output)))
        #:got (read-sexps "(list #$output)\n")))

    (test "`#$@` splices a list"
      (assert-equal #:expect (list (list (prefix "#$@") '(list 1)))
        #:got (read-sexps "#$@(list 1)\n")))

    (test "`#+` and `#+@` are gexp prefixes in Scheme"
      (assert-equal #:expect (list (list (prefix "#+") 'coreutils))
        #:got (read-sexps "#+coreutils\n"))
      (assert-equal #:expect (list (list (prefix "#+@") '(list 1)))
        #:got (read-sexps "#+@(list 1)\n")))

    (test "`#+` stays a conditional in Common Lisp"
      (assert-equal #:expect '()
        #:got (read-errors "#+sbcl (print 1)\n" #:dialect 'cl)))

    (test "a prefix is glued to its datum, and none is followed by a space"
      (assert-same-text #:expect "(list #$output #$@(list 1))\n"
                        #:got (formatted "(list #$output #$@ (list 1))\n")))

    (test "a formatted gexp is left alone"
      (assert-same-text #:expect "#~(use-modules (guix build utils))\n"
                        #:got (formatted "#~(use-modules (guix build utils))\n")))

    (test "the body of a gexp is read as code"
      (assert-warnings #:source "#~(if a b)\n"
                       #:checker 'if-without-else
                       #:expect '((1 4))))

    (test "formatting a gexp is a fixed point"
      (assert-idempotent
       #:source "#~(mkdir #$output #$(file-append coreutils \"/bin/ls\"))\n"))))
