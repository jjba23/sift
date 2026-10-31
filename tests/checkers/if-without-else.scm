;; sift --- checker: if-without-else
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; An `if` with no else branch is a `when` spelled the long way.  The fix
;; rewrites the operator, which is the whole change.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers if-without-else)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: if-without-else"

    (test "reports an if with two arguments"
      (assert-warnings #:source "(if a b)\n" #:checker 'if-without-else
        #:expect '((1 2))))

    (test "accepts an if with an else branch"
      (assert-warnings #:source "(if a b c)\n" #:checker 'if-without-else
        #:expect '()))

    (test "the fix rewrites it as when"
      (assert-fix #:source "(if a b)\n" #:checker 'if-without-else
        #:expected "(when a b)\n"))

    (test "the fixed file has nothing left to report"
      (assert-fixes-to-nothing #:source "(if a b)\n" #:checker 'if-without-else))))
