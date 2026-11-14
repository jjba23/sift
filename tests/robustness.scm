;; sift --- odd inputs
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A linter meets every file in a repository sooner or later, including the
;; half-edited one, the one with a stray byte, and the one that is a list of
;; symbols where a definition was expected.  None of them may make it raise:
;; a tool that crashes on input it did not expect is a tool that reports
;; nothing about the file that needed it most.
;;
;; Every case here is a source, not an assertion about a finding: what is
;; checked is that the whole pipeline -- read, every rule, format, and the
;; fixes -- returns an answer.

(define-module (tests robustness)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (veritas veritas)
  #:use-module (sift reader)
  #:use-module (sift format)
  #:use-module (sift lint)
  #:use-module (sift checkers bodies)
  #:use-module (tests helpers)
  #:export (spec))

(define odd-sources
  '(""
    " "
    "\n\n\n"
    "("
    ")"
    "())"
    "(defun)"
    "(defun defmacro)"
    "(defun f)"
    "(defun f ()"
    "(defmacro)"
    "(let)"
    "(let ())"
    "(case)"
    "(case x)"
    "(case x ())"
    "(case x ((cond) 1))"
    "(cond)"
    "(cond ())"
    "(lambda)"
    "(lambda ())"
    "(define)"
    "(define ())"
    "(quote)"
    "'"
    "#"
    "#;"
    "#|"
    "#\\"
    "?\\"
    "(a . )"
    "(a . b . c)"
    "(#)"
    "(list #;)"
    "(list #|x|#)"
    "(((("
    "))))"
    "(f :a 1 :a 2)"
    "(f \"unterminated"
    "(f #<garbage)"
    "(f #.(+ 1 2))"
    "(let ((x 1) (x 2)) x)"
    "(when)"
    "(unless)"
    "(if)"
    "(do)"
    "(λ (x) x)"))

(define dialects '(scheme racket elisp cl))

(define (spec)
  (suite "robustness"

    (test "every odd source is read, linted and formatted without raising"
      (map (lambda (dialect)
             (map (lambda (source)
                    (let* ((label (format #f "~a: ~s" dialect source))
                           (warnings (lint-source source #:dialect dialect))
                           (formatted (format-string source #:dialect dialect))
                           (fixed (apply-fixes source warnings)))
                      (assert-true (list? warnings) #:name label)
                      (assert-true (string? formatted) #:name label)
                      (assert-true (string? fixed) #:name label)))
                  odd-sources))
           dialects))

    (test "formatting an odd source twice changes nothing"
      (map (lambda (source)
             (let* ((once (format-string source))
                    (twice (format-string once)))
               (assert-same-text #:expect once #:got twice
                                 #:name (format #f "idempotent: ~s" source))))
           odd-sources))

    (test "a case clause whose datum is a list of symbols is not a form"
      ;; this crashed once: `(defun defmacro)` inside a memq list looked like
      ;; a defun to the rules, and a defun with two elements has no body
      (assert-equal #:expect '()
        #:got (lint-source "(memq head '(defun defmacro))"
                           #:dialect 'scheme
                           #:checkers '(docstring-missing empty-body
                                                          docstring-before-interactive))))

    (test "an empty body is reported rather than raising"
      (assert-equal #:expect 1
        #:got (length (lint-source "(defun f ())" #:dialect 'elisp
                                   #:checkers '(docstring-missing)))))))
