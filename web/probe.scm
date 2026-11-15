;; sift --- the browser build, asked every question it answers
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `maak web' says the module compiles.  It cannot say the module *runs*: a
;; miscompiled module compiles, loads, and then dies on the first request that
;; reaches the bad code -- which is how `lint' and `hover' came to answer
;; `WASM runtime error: invalid cast' in the tab while the gate was green.
;;
;; This program is that missing question.  It is compiled by the same compiler
;; with the same flags and run in Hoot's own virtual machine, and it asks the
;; facade every operation the protocol has, one JSON request each, judging the
;; answers the way the page needs them to be judged: the request is answered,
;; the shape is the shape the protocol promises, and the fact the operation is
;; for is in it.
;;
;; It reports one line and it is a *verdict*, not a transcript: `ok <n>' or
;; `failed <op>: <what>'.  What it is not is a second copy of tests/web.scm --
;; that suite asserts what sift *means*, under Guile, in far more detail than
;; this could afford.  What is asserted here is only what a tab would notice
;; within a second of opening the page, which is exactly what a miscompilation
;; takes away.
;;
;; Run it with `web/check.sh', or by hand:
;;
;;   guild compile-wasm --run -O2 -Ono-contify \
;;     -L src -L web -L web/compat web/probe.scm

(import (scheme base)
  (scheme write)
  (sift json)
  (sift web))

;;; ===== Asking, and judging =====

(define failures '())

(define (fail! op what)
  (set! failures (cons (string-append op " (" what ")") failures)))

(define (ask op request)
  "Return the answer to @var{request}, or #f when it is not an answer at all."
  (guard (condition (#t (fail! op "the request was not answered") #f))
    (json-read (web-request (json-write request)))))

(define (answered? op request test)
  "Ask @var{request} and run @var{test} on its result; say so when it fails."
  (let ((answer (ask op request)))
    (cond
      [(not answer) #f]
      [(not (eq? (json-ref answer "ok") json-true))
       (fail! op (string-append "refused: "
                                (json-ref (json-ref answer "error") "message")))
       #f]
      [else
        (let ((result (json-ref answer "result")))
          (unless (test result) (fail! op "the answer does not hold what it should"))
          result)])))

(define (count-of value)
  (if (vector? value) (vector-length value) 0))

(define (non-empty? value)
  (and (string? value) (> (string-length value) 0)))

;;; ===== Every operation the protocol has =====
;;;
;;; One request each, on sources small enough to reason about, and one fact
;;; asserted per operation: the rule that fires, the name that is found, the
;;; two arguments that are named.  A source that answers nothing is the shape a
;;; miscompilation takes, so the facts are what make the answer worth reading.

(define (probe)
  (answered? "version"
             (json-object (cons "op" "version"))
             (lambda (result) (non-empty? (json-ref result "version"))))

  ;; `checkers' is deliberately not asked here.  It is one request that builds
  ;; the whole rule catalogue and serialises it -- a hundred kilobytes -- and
  ;; in Hoot's own interpreter that is minutes rather than the fifty
  ;; milliseconds a browser takes, which is a cost this check would pay on
  ;; every run for an answer `tests/web.scm' already asserts in full under
  ;; Guile.  The operations below are the ones a page uses on every keystroke.
  (answered? "sample"
             (json-object (cons "op" "sample") (cons "dialect" "racket"))
             (lambda (result) (non-empty? (json-ref result "source"))))

  (answered? "format"
             (json-object (cons "op" "format")
                          (cons "source" "(define (f x)\n(+ x 1))\n"))
             (lambda (result) (string=? (json-ref result "source")
                                        "(define (f x)\n  (+ x 1))\n")))

  (answered? "check"
             (json-object (cons "op" "check")
                          (cons "source" "(define (f x)\n(+ x 1))\n"))
             (lambda (result) (eq? (json-ref result "changed") json-true)))

  ;; `unused-argument' and `single-letter-name' both point at the `x' of the
  ;; parameter list, which is the pair of findings the page opens on
  (answered? "lint"
             (json-object (cons "op" "lint")
                          (cons "source" "(define (f x) 1)\n"))
             (lambda (result)
               (and (= 2 (count-of (json-ref result "warnings")))
                 (= 2 (json-ref (json-ref result "counts") "info")))))

  (answered? "fix"
             (json-object (cons "op" "fix")
                          (cons "source" "(define (f) (let ((x 1)) x))"))
             (lambda (result)
               (and (= 1 (json-ref result "applied"))
                 (string=? (json-ref result "source") "(define (f) 1)"))))

  (answered? "extract"
             (json-object (cons "op" "extract")
                          (cons "source" "(define (f) (let ((x 1)) x))\n")
                          (cons "offset" 23))
             (lambda (result) (eq? (json-ref result "changed") json-true)))

  (answered? "hover"
             (json-object (cons "op" "hover")
                          (cons "source" "(define (f x) 1)\n")
                          (cons "offset" 9))
             (lambda (result) (json-ref result "hover")))

  (answered? "definition"
             (json-object (cons "op" "definition")
                          (cons "source" "(define (f x) 1)\n")
                          (cons "offset" 9))
             (lambda (result) (= 1 (count-of (json-ref result "definitions")))))

  (answered? "references"
             (json-object (cons "op" "references")
                          (cons "source" "(define (f x) 1)\n(f 2)\n")
                          (cons "offset" 9))
             (lambda (result) (= 2 (count-of (json-ref result "references")))))

  (answered? "highlight"
             (json-object (cons "op" "highlight")
                          (cons "source" "(define (f x) 1)\n(f 2)\n")
                          (cons "offset" 9))
             (lambda (result) (> (count-of (json-ref result "highlights")) 0)))

  (answered? "symbols"
             (json-object (cons "op" "symbols")
                          (cons "source" "(define (f x) 1)\n"))
             (lambda (result)
               (let ((symbols (json-ref result "symbols")))
                 (and (= 1 (count-of symbols))
                   (string=? (json-ref (vector-ref symbols 0) "name") "f")))))

  (answered? "completion"
             (json-object (cons "op" "completion")
                          (cons "source" "(define (square x) x)\n(sq")
                          (cons "offset" 25))
             (lambda (result) (= 1 (count-of (json-ref result "items")))))

  (answered? "signature"
             (json-object (cons "op" "signature")
                          (cons "source" "(define (area w h) 0)\n(area")
                          (cons "offset" 27))
             (lambda (result) (json-ref result "signature")))

  (answered? "inlayHints"
             (json-object (cons "op" "inlayHints")
                          (cons "source" "(define (area w h) 0)\n(area 2 3)\n"))
             (lambda (result) (= 2 (count-of (json-ref result "hints")))))

  ;; a request that cannot be answered is an answer too: the page shows the
  ;; sentence rather than dying of a stack trace
  (let ((answer (ask "refusal" (json-object (cons "op" "no-such-op")))))
    (unless (and answer (eq? (json-ref answer "ok") json-false))
      (fail! "refusal" "an unknown op was not reported as an error")))

  (if (null? failures)
    "ok 16"
    (string-append "failed " (car (reverse failures))
                   (if (null? (cdr failures))
                     ""
                     (string-append " and " (number->string (length failures))
                                    " more")))))

;; The last expression is what the host gets: a string the check reads.
(probe)
