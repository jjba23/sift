;; sift --- checker: checkdoc-style-verb
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; The manual asks the first sentence to be in the imperative: "Return the
;; value", not "Returns the value".  This is informational, because it is a
;; matter of voice rather than of correctness.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers checkdoc-style-verb)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: checkdoc-style-verb"

    (test "reports a third-person opening"
      (assert-warnings #:source "(defun f (x)\n  \"Returns x.\"\n  x)\n"
        #:dialect 'elisp #:checker 'checkdoc-style-verb
        #:expect '((2 3))))

    (test "accepts the imperative"
      (assert-warnings #:source "(defun f (x)\n  \"Return x.\"\n  x)\n"
        #:dialect 'elisp #:checker 'checkdoc-style-verb
        #:expect '()))))
