;; sift --- formatter tests, Scheme
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The golden corpus is the specification of the formatter, and these are the
;; three passes over it: an input formats to its expected file, the expected
;; file is a fixed point of formatting, and a file of nothing but comments and
;; whitespace comes back byte for byte.
;;
;; Every assertion is named after the file it is about, so a failure says
;; which fixture disagrees before it says anything else.

(define-module (tests format-scheme)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift format)
  #:use-module (tests helpers)
  #:export (spec))

(define (scheme-pairs)
  (filter (lambda (pair)
            (and (eq? (dialect-of (car pair)) 'scheme)
              (eq? (style-of (car pair)) 'community)))
    (fixture-pairs)))

(define (dangling-pairs)
  (filter (lambda (pair)
            (and (eq? (dialect-of (car pair)) 'scheme)
              (eq? (style-of (car pair)) 'dangling)))
    (fixture-pairs)))

(define (spec)
  (suite "format: Scheme"

    (test "every fixture formats to its expected file"
      (map assert-fixture (scheme-pairs)))

    (test "every expected file is a fixed point of formatting"
      (map assert-fixture-idempotent (scheme-pairs)))

    (test "the dangling style puts each closer on its own line"
      (map assert-fixture (dangling-pairs))
      (map assert-fixture-idempotent (dangling-pairs)))

    (test "indentation follows the operator's rule"
      (assert-same-text
         #:expect "(define (f x)\n  (let ((a 1))\n    (if x\n      a\n      x)))\n"
         #:got (format-string "(define (f x)\n(let ((a 1))\n(if x\n a\n x)))\n")))

    (test "a data list lines up under its first element"
      (assert-same-text
         #:expect "(let ((a 1)\n      (b 2))\n  (+ a b))\n"
         #:got (format-string "(let ((a 1)\n(b 2))\n(+ a b))\n")))

    (test "an unknown operator with its first argument on the head line indents two"
      (assert-same-text
         #:expect "(my-macro a\n  b)\n"
         #:got (format-string "(my-macro a\n      b)\n")))

    (test "an unknown operator whose first argument is on the next line indents from the operator"
      (assert-same-text
         #:expect "(my-macro\n   a)\n"
         #:got (format-string "(my-macro\n a)\n")))

    (test "a blank line inside a form is kept, but a run is collapsed to two"
      (assert-same-text
         #:expect "(a\n\n\n   b)\n"
         #:got (format-string "(a\n\n\n\nb)\n")))

    (test "blank lines between top-level forms are collapsed to one"
      (assert-same-text
         #:expect "(a)\n\n(b)\n"
         #:got (format-string "(a)\n\n\n\n(b)\n")))

    (test "a reader prefix is never separated from its datum"
      (assert-same-text
         #:expect "'a\n"
         #:got (format-string "'   a\n")))

    (test "a comment keeps the line it was on"
      (assert-same-text
         #:expect "(a ; inline\n   b)\n"
         #:got (format-string "(a    ; inline\nb)\n")))

    (test "comments are aligned when asked"
      (assert-same-text
         #:expect "(let ((a 1)   ; one\n      (b 22)) ; two\n  (+ a b))\n"
         #:got (format-string "(let ((a 1) ; one\n      (b 22)) ; two\n  (+ a b))\n"
                 #:comment-align #t)))

    (test "strings are never reflowed or respelled"
      (assert-same-text
         #:expect "(display \"a   b\\n\")\n"
         #:got (format-string "(display    \"a   b\\n\")\n")))

    (test "formatting is idempotent on this sentence's own source"
      (assert-idempotent #:source "(define (f x) (progn (a) (b)))\n"))

    (test "cond clauses are written with square brackets"
      (assert-same-text
         #:expect "(cond [(< n 0) 'negative]\n      [(= n 0) 'zero]\n      [else 'positive])"
         #:got (format-string "(cond ((< n 0) 'negative)\n      ((= n 0) 'zero)\n      (else 'positive))")))

    (test "a project that writes parentheses keeps them"
      (assert-same-text
         #:expect "(cond ((< n 0) 'negative)\n      (else 'positive))"
         #:got (format-string "(cond [(< n 0) 'negative]\n      [else 'positive])"
                 #:brackets 'parens)))

    (test "preserving brackets leaves each clause as it was written"
      (assert-same-text
         #:expect "(cond ((< n 0) 1)\n      [else 2])"
         #:got (format-string "(cond ((< n 0) 1)\n      [else 2])"
                 #:brackets 'preserve)))

    (test "a cond whose clauses start on their own lines indents them two in"
      (assert-same-text
         #:expect "(cond\n  [(< n 0) 1]\n  [else 2])"
         #:got (format-string "(cond\n((< n 0) 1)\n(else 2))")))

    (test "a vector keeps its own opener whatever the bracket style is"
      (assert-same-text #:expect "#(1 2 3)"
        #:got (format-string "#(1  2  3)" #:brackets 'square)))

    (test "keyword arguments line up under the first argument"
      (assert-same-text
         #:expect "(make-window title\n             :width 10\n             :height 20)"
         #:got (format-string "(make-window title\n:width 10\n:height 20)")))

    (test "a call without keywords keeps the ordinary rule"
      (assert-same-text
         #:expect "(make-window title\n  width\n  height)"
         #:got (format-string "(make-window title\nwidth\nheight)")))

    (test "a project's own indent rule wins over the table"
      (assert-same-text
         #:expect "(with-my-thing x\n    y)"
         #:got (format-string "(with-my-thing x\ny)"
                 #:indent-rules '((with-my-thing . 4)))))

    (test "a project's own indent rule is idempotent"
      (assert-idempotent
         #:source "(with-my-thing x\ny)\n"
         #:name "the project rule formats the same way twice"))

    (test "a multi-line datum comment moves with the code around it"
      (assert-same-text
         #:expect "(define (f)\n  #;(ignore\n  this)\n  x)\n"
         #:got (format-string "(define (f)\n#;(ignore\nthis)\nx)\n")))

    (test "a datum comment that moves left keeps its internal shape"
      (assert-same-text
         #:expect "(let ((a 1))\n  #;(b\n      c)\n  a)\n"
         #:got (format-string "(let ((a 1))\n        #;(b\n            c)\n  a)\n")))

    (test "moving a verbatim block is idempotent"
      (assert-idempotent
         #:source "(define (f)\n#;(ignore\nthis)\nx)\n"
         #:name "the shifted block is the same on the second pass"))

    (test "a single-line datum comment is untouched"
      (assert-same-text #:expect "(a #;(b c) d)"
        #:got (format-string "(a  #;(b c)  d)")))

    (test "a file without a final newline keeps that shape"
      (assert-same-text #:expect "(a b)"
        #:got (format-string "(a  b)")))))
