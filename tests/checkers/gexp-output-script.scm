;; sift --- checker: gexp-output-script
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `#$output` is the directory of a derivation's output.  A `program-file' or
;; `gexp->script' makes one store *file*, so the builder reads `getenv "out"'
;; and gets false -- which is what the manual means when it says `#$output`
;; only makes sense for a gexp passed to `gexp->derivation'.

(define-module (tests checkers gexp-output-script)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: gexp-output-script"

    (test "reports `#$output` in a `program-file`"
      (assert-warnings #:source "(program-file \"run.sh\" #~(execl #$output \"x\" #f))\n"
                       #:checker 'gexp-output-script
                       #:expect '((1 33))))

    (test "reports `#$output` in a `gexp->script`"
      (assert-warnings #:source "(gexp->script \"run\" #~(execl #$output \"x\" #f))\n"
                       #:checker 'gexp-output-script
                       #:expect '((1 30))))

    (test "accepts `#$output` in a derivation"
      (assert-warnings #:source "(gexp->derivation \"x\" #~(mkdir #$output))\n"
                       #:checker 'gexp-output-script
                       #:expect '()))

    (test "accepts another ungexp in a file-making gexp"
      (assert-warnings #:source "(program-file \"run.sh\" #~(execl #$coreutils \"ls\" #f))\n"
                       #:checker 'gexp-output-script
                       #:expect '()))))
