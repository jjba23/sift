;; sift --- checker: save-excursion-set-buffer
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `save-excursion' saves point for the buffer that was current when it
;; started.  A `set-buffer' inside it changes which buffer `point' means, so
;; what the end of the form restores is the point of the buffer the body left
;; behind -- which is why the byte compiler warns about the shape.

(define-module (tests checkers save-excursion-set-buffer)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: save-excursion-set-buffer"

    (test "reports a set-buffer inside save-excursion"
      (assert-warnings #:source "(save-excursion (set-buffer b) (goto-char 1))\n"
                       #:checker 'save-excursion-set-buffer
                       #:dialect 'elisp
                       #:expect '((1 17))))

    (test "reports one nested in a body form"
      (assert-warnings #:source "(save-excursion (when b (set-buffer b)))\n"
                       #:checker 'save-excursion-set-buffer
                       #:dialect 'elisp
                       #:expect '((1 25))))

    (test "accepts an excursion that changes nothing"
      (assert-warnings #:source "(save-excursion (goto-char 1))\n"
                       #:checker 'save-excursion-set-buffer
                       #:dialect 'elisp
                       #:expect '()))

    (test "accepts with-current-buffer around an excursion"
      (assert-warnings
       #:source "(with-current-buffer b (save-excursion (goto-char 1)))\n"
       #:checker 'save-excursion-set-buffer
       #:dialect 'elisp
       #:expect '()))))
