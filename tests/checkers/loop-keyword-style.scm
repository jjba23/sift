;; sift --- checker: loop-keyword-style
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Some projects write LOOP keywords as symbols (`for`, `in`, `do`) and some
;; as keywords (`:for`, `:in`, `:do`).  The default accepts both; setting the
;; style makes the other one a finding with a fix.
;;
;; Every checker has a passing fixture and a failing one, and a fix test when
;; it has a fix: the fixed text is asserted, and so is the fact that the fix
;; leaves nothing behind for the next run.

(define-module (tests checkers loop-keyword-style)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: loop-keyword-style"

    (test "says nothing when both spellings are accepted"
      (assert-warnings #:source "(loop for x in xs do (print x))\n"
                       #:dialect 'cl #:checker 'loop-keyword-style
                       #:expect '()))

    (test "reports symbolic keywords when the style asks for literal ones"
      (assert-warnings #:source "(loop for x in xs do (print x))\n"
                       #:dialect 'cl #:checker 'loop-keyword-style
                       #:options '((loop-keywords . literal))
                       #:expect '((1 7) (1 13) (1 19))))

    (test "the fix rewrites the keywords"
      (assert-fix #:source "(loop for x in xs do (print x))\n"
                  #:dialect 'cl #:checker 'loop-keyword-style
                  #:options '((loop-keywords . literal))
                  #:expected "(loop :for x :in xs :do (print x))\n"))))
