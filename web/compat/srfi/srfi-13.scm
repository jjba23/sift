;; sift --- SRFI-13, as much of it as sift uses
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; SRFI-13 is a large library and Guile carries all of it, so the command line
;; -- and every editor beside it -- asks Guile for these procedures and gets
;; them.  Hoot does not carry SRFI-13: its own library directory has srfi-9,
;; srfi-11 and srfi-14 and stops there, and Guile's own `srfi/srfi-13.scm' is
;; written with `(re-export ...)', a syntax Hoot does not implement.  A build
;; that reached for it would compile a re-export of names the target does not
;; define, and fail by name.
;;
;; So this file is the library for the browser build: the thirteen procedures
;; sift actually calls, written against `(scheme base)' and nothing else, in
;; SRFI-13's argument order, with SRFI-13's answers.  It sits in web/compat, so
;; it is found by a build that puts this directory first on the load path and
;; by no other build: `bin/sift' loads Guile's SRFI-13 and every byte of its
;; behaviour is what it always was.
;;
;; Two limits are worth stating rather than discovering:
;;
;;   - a char-set is not accepted.  A character set is an object R7RS does not
;;     have, so the trim and index procedures below take a character or a
;;     predicate, and sift's three call sites that passed a `char-set' pass a
;;     predicate instead -- which Guile reads the same way.
;;   - the optional string ranges are not accepted, for the same reason the
;;     set is not: nothing in sift passes them, and a parameter that is never
;;     exercised is a parameter that is wrong quietly.
;;
;; `string-upcase' is here because `(guile)' exports it and does not define it:
;; Hoot's shim has no case mapping above characters, and `(scheme char)' maps
;; characters only.  The copy below is ASCII, which is what the one caller
;; wants -- the JSON writer's escape for a control character, hex digits and
;; nothing else -- and it is spelled out so that a wider caller has to come
;; back here and say so.

(define-module (srfi srfi-13)
  #:declarative? #t
  #:export (string-contains
            string-drop-right
            string-every
            string-index
            string-join
            string-null?
            string-pad-right
            string-prefix?
            string-suffix?
            string-trim
            string-trim-right
            string-trim-both
            string-upcase))

;;; ===== Asking about one character =====
;;;
;;; SRFI-13 lets the trim and index procedures take a character or a predicate,
;;; and sift uses both -- `(string-index text #\newline)' for a character and
;;; `(string-index line (lambda (c) ...))' for a question.  One procedure that
;;; answers both is what keeps this file to a page.

(define (character-wanted? wanted character)
  "Return @code{#t} when @var{character} is what @var{wanted} asks for.

@var{wanted} is a character, or a predicate of one character.  Anything else is
a mistake the caller made, and it is said so rather than answered around."
  ;; `(procedure? wanted)' is deliberately not written here.  Hoot infers the
  ;; type of the value from that test and emits a cast to the procedure type it
  ;; inferred, which fails at run time for a predicate that arrives as a lambda
  ;; -- the browser build answered every such call with "illegal cast", which
  ;; is what turned every file beginning with a `;' comment into an editor that
  ;; stopped answering.  Asking the question with `char?' alone, and letting the
  ;; call itself be the test, is the same answer without the cast.
  (if (char? wanted)
    (char=? wanted character)
    (and (wanted character) #t)))

(define (default-space? character)
  "Return @code{#t} when @var{character} is one of the four spaces SRFI-13 trims."
  (memv character '(#\space #\tab #\newline #\return)))

;;; ===== The trims =====

;; The trimming itself is written for one @var{wanted} at a time, and the
;; variadic entry points below hand it the predicate or the character they
;; were given.  That split is not a style: Hoot emits a cast for a call of a
;; variadic procedure that is made through `apply', and for a variable that
;; holds either a character or a procedure, and both casts failed at run time
;; with "illegal cast".  Every trim in the browser build -- and so every file
;; that begins with a `;' comment, which is what `sift:' directives are read
;; out of -- answered with that trap.

(define (trim-right wanted string)
  "Return @var{string} without the spaces @var{wanted} asks for, on its right."
  (let ((length (string-length string)))
    (let loop ((end length))
      (if (and (> end 0)
            (character-wanted? wanted (string-ref string (- end 1))))
        (loop (- end 1))
        (if (= end length) string (substring string 0 end))))))

(define (trim-left wanted string)
  "Return @var{string} without the spaces @var{wanted} asks for, on its left."
  (let* ((length (string-length string))
         (start (let loop ((start 0))
                  (if (and (< start length)
                        (character-wanted? wanted (string-ref string start)))
                    (loop (+ start 1))
                    start))))
    (if (= start 0) string (substring string start length))))

(define (string-trim-right string . rest)
  "Return @var{string} without the spaces on its right.

The optional argument is the character to remove, or a predicate that says
which characters are spaces; with none, a space, a tab, a newline and a return
are all spaces."
  (if (pair? rest)
    (trim-right (car rest) string)
    (trim-right default-space? string)))

(define (string-trim string . rest)
  "Return @var{string} without the spaces on its left.

The optional argument is SRFI-13's: a character, or a predicate."
  (if (pair? rest)
    (trim-left (car rest) string)
    (trim-left default-space? string)))

(define (string-trim-both string . rest)
  "Return @var{string} without the spaces on either side of it."
  (if (pair? rest)
    (trim-left (car rest) (trim-right (car rest) string))
    (trim-left default-space? (trim-right default-space? string))))

;;; ===== Asking where something is =====

(define (string-index string wanted)
  "Return the index of the first character of @var{string} @var{wanted} asks
for, or @code{#f} when there is none.

@var{wanted} is a character or a predicate of one, the way SRFI-13's is."
  (let ((length (string-length string)))
    (let loop ((index 0))
      (cond
        [(>= index length) #f]
        [(character-wanted? wanted (string-ref string index)) index]
        [else (loop (+ index 1))]))))

(define (string-contains string needle)
  "Return the index of the first @var{needle} in @var{string}, or @code{#f}."
  (let ((length (string-length string))
        (width (string-length needle)))
    (if (> width length)
      #f
      (let loop ((start 0))
        (cond
          [(> (+ start width) length) #f]
          [(string=? needle (substring string start (+ start width))) start]
          [else (loop (+ start 1))])))))

;;; ===== Building one string from others =====

(define (string-join strings . rest)
  "Return @var{strings} joined with the delimiter, a space by default."
  (let ((delimiter (if (pair? rest) (car rest) " ")))
    (cond
      [(null? strings) ""]
      [(null? (cdr strings)) (car strings)]
      [else
        (let loop ((out (car strings)) (rest (cdr strings)))
          (if (null? rest)
            out
            (loop (string-append out delimiter (car rest)) (cdr rest))))])))

(define (string-pad-right string width . rest)
  "Return @var{string} widened to @var{width} columns by @var{char} on the right.

The character is a space by default, and a string already at least @var{width}
columns wide is answered as it came."
  (let ((character (if (pair? rest) (car rest) #\space))
        (length (string-length string)))
    (if (<= width length)
      string
      (string-append string (make-string (- width length) character)))))

;;; ===== Asking about a whole string =====

(define (string-null? string)
  "Return @code{#t} when @var{string} holds nothing."
  (zero? (string-length string)))

(define (string-prefix? prefix string)
  "Return @code{#t} when @var{prefix} is the beginning of @var{string}."
  (and (<= (string-length prefix) (string-length string))
    (string=? prefix (substring string 0 (string-length prefix)))))

(define (string-suffix? suffix string)
  "Return @code{#t} when @var{suffix} is the end of @var{string}."
  (let ((width (string-length suffix))
        (length (string-length string)))
    (and (<= width length)
      (string=? suffix (substring string (- length width) length)))))

(define (string-every predicate string)
  "Return the last value @var{predicate} answered for a character of @var{string}.

An empty @var{string} answers @code{#t}, and a @var{predicate} that answers
@code{#f} stops the walk and is the answer -- SRFI-13's rule, kept because a
caller that checks the answer with `and' expects it."
  (let ((length (string-length string)))
    (let loop ((index 0) (answer #t))
      (if (>= index length)
        answer
        (let ((answer (predicate (string-ref string index))))
          (and answer (loop (+ index 1) answer)))))))

(define (string-upcase string)
  "Return @var{string} in upper case.

ASCII only: the one caller upper-cases the hex digits of a JSON escape, and a
wider mapping would need the case tables `(scheme char)' does not carry."
  (list->string (map char-upcase (string->list string))))

;;; ===== Shortening a string =====

(define (string-drop-right string count)
  "Return @var{string} without its last @var{count} characters.

A @var{count} past the end of @var{string} is an error rather than an empty
string, which is what SRFI-13 does and what a caller that computed the count
from the string wants to hear about."
  (let ((end (- (string-length string) count)))
    (if (< end 0)
      (error "sift: string-drop-right drops more than the string holds" string count)
      (substring string 0 end))))
