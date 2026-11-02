;; sift --- checker: require-cl
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The `cl' library is deprecated: the Emacs Lisp Coding Conventions ask for
;; `cl-lib'.  Loading `cl' also defines the old unprefixed names for the whole
;; session, which is the part that outlives the file.

(define-module (tests checkers require-cl)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: require-cl"

    (test "reports a require of cl"
      (assert-warnings #:source "(require 'cl)\n"
                       #:checker 'require-cl
                       #:dialect 'elisp
                       #:expect '((1 1))))

    (test "reports the pieces of cl as well"
      (assert-warnings #:source "(require 'cl-macs)\n"
                       #:checker 'require-cl
                       #:dialect 'elisp
                       #:expect '((1 1))))

    (test "accepts cl-lib"
      (assert-warnings #:source "(require 'cl-lib)\n"
                       #:checker 'require-cl
                       #:dialect 'elisp
                       #:expect '()))

    (test "accepts another library"
      (assert-warnings #:source "(require 'subr-x)\n"
                       #:checker 'require-cl
                       #:dialect 'elisp
                       #:expect '()))))
