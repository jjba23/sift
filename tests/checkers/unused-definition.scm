;; sift --- checker: unused-definition
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A module that lists what it exports has said what its public surface is, and
;; another module cannot name anything that is not on that list.  So a top-level
;; definition that is neither exported nor mentioned in its own file is code
;; nothing can reach -- and the tests state the three ways that can be wrong if
;; the rule is careless: a file with no `#:export' (there is no surface to
;; compare against), a name under a quote (a mention a formatter cannot rule
;; out), and a definition another one calls.

(define-module (tests checkers unused-definition)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (sift lint)
  #:use-module (tests helpers)
  #:export (spec))

(define (spec)
  (suite "checker: unused-definition"

    (test "reports a definition nothing uses and the module does not export"
      (assert-warnings
       #:source "(define-module (demo)\n  #:export (used))\n(define (used) 1)\n(define (forgotten) 2)\n"
       #:checker 'unused-definition #:expect '((4 10))))

    (test "an exported definition is the module's surface"
      (assert-warnings
       #:source "(define-module (demo)\n  #:export (run))\n(define (run) 1)\n"
       #:checker 'unused-definition #:expect '()))

    (test "a definition another one uses is not dead"
      (assert-warnings
       #:source "(define-module (demo)\n  #:export (run))\n(define (helper) 1)\n(define (run) (helper))\n"
       #:checker 'unused-definition #:expect '()))

    (test "a name mentioned under a quote counts as mentioned"
      ;; the rule cannot know whether a macro will use the name, so an
      ;; appearance it cannot read is enough to leave the definition alone
      (assert-warnings
       #:source "(define-module (demo)\n  #:export (run))\n(define (helper) 1)\n(define (run) '(helper))\n"
       #:checker 'unused-definition #:expect '()))

    (test "a file with no define-module is not judged"
      (assert-warnings
       #:source "(define (forgotten) 1)\n"
       #:checker 'unused-definition #:expect '()))

    (test "a module with no #:export clause is not judged"
      ;; a script exports nothing and is right to, which is not the same as
      ;; saying that it exports nothing
      (assert-warnings
       #:source "(define-module (demo))\n(define (forgotten) 1)\n"
       #:checker 'unused-definition #:expect '()))

    (test "a module that says it exports nothing is judged"
      (assert-warnings
       #:source "(define-module (demo)\n  #:export ())\n(define (forgotten) 1)\n"
       #:checker 'unused-definition #:expect '((3 10))))

    (test "the name in the export list is the name that counts"
      ;; a `rename' in an export list publishes the second half, so the first
      ;; half is not an exported definition
      (assert-warnings
       #:source "(define-module (demo)\n  #:export (public))\n(define (private) 1)\n"
       #:checker 'unused-definition #:expect '((3 10))))))
