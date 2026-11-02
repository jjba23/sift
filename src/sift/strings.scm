;; sift --- the string procedures sift keeps for itself
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Three procedures that every Scheme programmer reaches for and that R7RS
;; does not have: find a substring, ask a question of every character, widen a
;; field.  They are SRFI-13's, spelled `string-contains', `string-every' and
;; `string-pad-right' there, and SRFI-13 is a large library that Guile carries
;; in full -- but not every platform sift is built for does.
;;
;; So they are written here against `string-ref', `char=?' and `string-append',
;; and they are spelled with names of sift's own.  That is the part worth
;; saying out loud: reusing the SRFI-13 names would work, and would make every
;; module that wants one of them carry an import clause saying "not that one"
;; -- which is exactly the kind of instruction that gets dropped by the next
;; person to add an import.  A name nobody else claims costs one word at the
;; call site and asks nothing of the reader.
;;
;; The optional arguments are SRFI-13's, in SRFI-13's order, so that a caller
;; who knows the SRFI already knows these.

(define-module (sift strings)
  #:declarative? #t
  #:export (string-find
            every-char
            pad-right
            string-words))

;;; ===== The optional ranges =====

(define (checked-end string name start end)
  "Return END, or the end of @var{string} when it is @code{#f}, or stop."
  (let ((end (or end (string-length string))))
    (if (<= start end)
      end
      (error "sift: a string range ends before it starts" name start end))))

(define (range-arguments rest count)
  "Return at most @var{count} of the optional arguments in @var{rest}."
  (let loop ((i 0) (rest rest) (out '()))
    (if (or (>= i count) (not (pair? rest)))
      (reverse out)
      (loop (+ i 1) (cdr rest) (cons (car rest) out)))))

(define (argument arguments index default)
  "Return the @var{index}-th of @var{arguments}, or @var{default}."
  (if (< index (length arguments)) (list-ref arguments index) default))

;;; ===== Finding a substring =====

(define (string-find string needle . rest)
  "Return the index of the first @var{needle} in @var{string}, or @code{#f}.

The optional arguments are ranges: the part of @var{string} to search, then
the part of @var{needle} to look for.  An empty @var{needle} is found at the
start of the range, so that `(string-find text \"\")' is a question about the
range and not a special case.

The answer is an index, and 0 is a true value, so a caller asking only whether
a string occurs can use it as a predicate -- which is what most of sift does."
  (let* ((arguments (range-arguments rest 4))
         (start (argument arguments 0 0))
         (end (checked-end string 'string-find start (argument arguments 1 #f)))
         (needle-start (argument arguments 2 0))
         (needle-end (checked-end needle 'string-find needle-start
                                  (argument arguments 3 #f)))
         (needle-length (- needle-end needle-start)))
    (let loop ((index start))
      (cond
        [(> (+ index needle-length) end) #f]
        [(let inner ((offset 0))
           (cond
             [(>= offset needle-length) #t]
             [(char=? (string-ref string (+ index offset))
                      (string-ref needle (+ needle-start offset)))
              (inner (+ offset 1))]
             [else #f]))
         index]
        [else (loop (+ index 1))]))))

;;; ===== Asking about every character =====

(define (every-char predicate string . rest)
  "Return the last value @var{predicate} returned, or @code{#t} for no characters.

A character may be given in place of a predicate and stands for the predicate
that asks whether a character is that one, which is the SRFI-13 shorthand.
A predicate that answers @code{#f} once ends the walk, so a caller asking
whether a name is all punctuation does not read the rest of the name."
  (let* ((arguments (range-arguments rest 2))
         (start (argument arguments 0 0))
         (end (checked-end string 'every-char start (argument arguments 1 #f)))
         (test (if (char? predicate)
                 (lambda (char) (char=? char predicate))
                 predicate)))
    (let loop ((index start) (answer #t))
      (if (>= index end)
        answer
        (let ((result (test (string-ref string index))))
          (and result (loop (+ index 1) result)))))))

;;; ===== Words =====

(define (ascii-letter? char)
  (or (char<=? #\a char #\z) (char<=? #\A char #\Z)))

(define (ascii-digit? char)
  (char<=? #\0 char #\9))

(define (word-character? char)
  "Return @code{#t} for a character a Lisp name is made of, ASCII only.

The set is exactly the letters, the digits, `-' and `_': a docstring is read
by a person and by `checkdoc', and a word boundary is what they both mean by
a word.  It is spelled out rather than taken from `char-set' because a
character set is an object a Scheme may not have, and R7RS is what this module
is written against."
  (or (ascii-letter? char) (ascii-digit? char)
    (char=? char #\-) (char=? char #\_)))

(define (string-words text)
  "Return the runs of word characters in @var{text}, in the order written.

`(string-words \"Add  two numbers.\")' is @code{(\"Add\" \"two\" \"numbers\")}."
  (let loop ((chars (string->list text)) (current '()) (words '()))
    (define (finish words)
      (if (null? current)
        words
        (cons (list->string (reverse current)) words)))
    (cond
      [(null? chars) (reverse (finish words))]
      [(word-character? (car chars))
       (loop (cdr chars) (cons (car chars) current) words)]
      [else (loop (cdr chars) '() (finish words))])))

;;; ===== Widening a field =====

(define (pad-right string width . rest)
  "Return @var{string} padded on the right with @var{char} to @var{width}.

The table `list-checkers' prints is laid out with this.  A field wider than
its column is not truncated: it is the author's text and not the table's, and
a formatter that cut a description to fit a column would be lying about it."
  (let* ((arguments (range-arguments rest 3))
         (char (argument arguments 0 #\space))
         (start (argument arguments 1 0))
         (end (checked-end string 'pad-right start (argument arguments 2 #f)))
         (text (substring string start end))
         (missing (- width (string-length text))))
    (if (> missing 0)
      (string-append text (make-string missing char))
      text)))
