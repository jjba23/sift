;; sift --- checker: phase-name-quoting
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `modify-phases' names phases by symbol.  `(delete configure)' deletes the
;; value of a variable called `configure', which in a build script is rarely
;; what the author wrote -- and always a mistake when it is.

(define-module (tests checkers phase-name-quoting)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: phase-name-quoting"

    (test "reports an unquoted phase name"
      (assert-warnings #:source "(modify-phases %standard-phases (delete configure))\n"
                       #:checker 'phase-name-quoting
                       #:expect '((1 41))))

    (test "the fix quotes it"
      (assert-fix #:source "(modify-phases %standard-phases (delete configure))\n"
                  #:checker 'phase-name-quoting
                  #:expected "(modify-phases %standard-phases (delete 'configure))\n")
      (assert-fixes-to-nothing #:source "(modify-phases %standard-phases (delete configure))\n"
                               #:checker 'phase-name-quoting))

    (test "reports a phase named by a string"
      (assert-warnings #:source "(modify-phases %standard-phases (replace \"configure\" (lambda _ #t)))\n"
                       #:checker 'phase-name-quoting
                       #:expect '((1 42))))

    (test "accepts quoted names in `add-before` and `add-after`"
      (assert-warnings #:source "(modify-phases %standard-phases\n  (add-before 'configure 'build (lambda _ #t))\n  (add-after 'build 'check (lambda _ #t)))\n"
                       #:checker 'phase-name-quoting
                       #:expect '()))

    (test "leaves a `replace` outside `modify-phases` alone"
      (assert-warnings #:source "(delete configure)\n"
                       #:checker 'phase-name-quoting
                       #:expect '()))))
