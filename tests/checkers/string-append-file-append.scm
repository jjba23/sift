;; sift --- checker: string-append-file-append
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The manual draws the difference between `string-append' and `file-append'
;; in one sentence: the first inserts the package's directory and builds the
;; file name at run time, the second puts the whole absolute file name in the
;; store, where `grep' and the closure can see it.

(define-module (tests checkers string-append-file-append)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: string-append-file-append"

    (test "reports a run-time `string-append` in a gexp"
      (assert-warnings #:source "#~(system* (string-append #$coreutils \"/bin/ls\"))\n"
                       #:checker 'string-append-file-append
                       #:expect '((1 12))))

    (test "the fix writes `file-append`"
      (assert-fix #:source "#~(system* (string-append #$coreutils \"/bin/ls\"))\n"
                  #:checker 'string-append-file-append
                  #:expected "#~(system* #$(file-append coreutils \"/bin/ls\"))\n")
      (assert-fixes-to-nothing #:source "#~(system* (string-append #$coreutils \"/bin/ls\"))\n"
                               #:checker 'string-append-file-append))

    (test "accepts `file-append`"
      (assert-warnings #:source "#~(system* #$(file-append coreutils \"/bin/ls\"))\n"
                       #:checker 'string-append-file-append
                       #:expect '()))

    (test "accepts the output directory, which is not a package"
      (assert-warnings #:source "#~(chdir (string-append #$output \"/share\"))\n"
                       #:checker 'string-append-file-append
                       #:expect '()))

    (test "leaves a `string-append` outside a build body alone"
      (assert-warnings #:source "(string-append #$coreutils \"/bin/ls\")\n"
                       #:checker 'string-append-file-append
                       #:expect '()))

    ;; In a quasiquoted build body the file name is still built at run time,
    ;; and the finding says so; the fix is not offered, because turning the
    ;; body into a gexp is the author's rewrite and not a replacement of one
    ;; form.
    (test "reports it in a quasiquoted build body"
      (assert-warning-count
       #:source "(gexp->derivation \"x\" `(system* (string-append ,coreutils \"/bin/ls\")))\n"
       #:checker 'string-append-file-append
       #:count 1))

    (test "and leaves that body as it found it"
      (assert-fix
       #:source "(gexp->derivation \"x\" `(system* (string-append ,coreutils \"/bin/ls\")))\n"
       #:checker 'string-append-file-append
       #:expected "(gexp->derivation \"x\" `(system* (string-append ,coreutils \"/bin/ls\")))\n"))))
