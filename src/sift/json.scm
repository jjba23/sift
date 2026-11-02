;; sift --- JSON
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The JSON a language server speaks, read and written here rather than
;; borrowed: sift depends on Guile and nothing else, and `(json)' is another
;; package to install before a formatter will start.
;;
;; The two shapes that a Lisp does not have are the two that matter:
;;
;;   an object  is `(object ("key" . value) ...)`, in the order written
;;   an array   is a vector, so `(vector)` is the empty one
;;
;; A pair whose car is `object' cannot be confused with an array, an empty
;; object cannot be confused with an empty array, and the order of a JSON
;; object survives the round trip -- which is what a caller comparing two
;; documents needs, since a hash table promises nothing about order.
;;
;; `null', `true' and `false' are the symbols `null', `true' and `false', so
;; that `#f' means what it means in Scheme and a JSON `false' is not the
;; absence of a value.

(define-module (sift json)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (sift strings)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (json-read
            json-write
            json-object
            json-object?
            json-fields
            json-ref
            json-ref-path
            json-get
            json-array?
            json-null?
            json-null
            json-true
            json-false
            json-number->string))

;;; ===== The values =====

(define json-null 'null)
(define json-true 'true)
(define json-false 'false)
(define json-object-tag 'object)

(define (json-object . fields)
  "Return a JSON object of @var{fields}, @code{(name . value)} pairs."
  (cons json-object-tag fields))

(define (json-object? value)
  "Return @code{#t} when @var{value} is a JSON object."
  (and (pair? value) (eq? (car value) json-object-tag)))

(define (json-fields value)
  "Return the @code{(name . value)} pairs of a JSON object, in order."
  (and (json-object? value) (cdr value)))

(define (json-ref object name . rest)
  "Return the member @var{name} of @var{object}, or the optional default.

The name is a string, the way JSON writes it; a symbol is accepted too,
because a caller writing a request by hand has no reason to type quotes."
  (let ((key (if (symbol? name) (symbol->string name) name))
        (default (if (pair? rest) (car rest) #f)))
    (let ((entry (and (json-object? object)
                   (assoc key (cdr object)))))
      (if entry (cdr entry) default))))

(define (json-ref-path object . names)
  "Return the member at @var{names}, a path of keys, or @code{#f}."
  (fold (lambda (name value) (json-ref value name)) object names))

(define (json-get name object . rest)
  "Return the member @var{name} of @var{object}, or the optional default.

This is `json-ref' with the argument order a fold or a `map' wants."
  (apply json-ref object name rest))

(define (json-array? value)
  "Return @code{#t} when @var{value} is a JSON array."
  (vector? value))

(define (json-null? value)
  (eq? value json-null))

;;; ===== Reading =====
;;;
;;; A character-at-a-time reader over a string, in the manner of the Lisp one
;;; in (sift reader): the position is an index, and every procedure takes the
;;; state it needs.  It is strict where the specification is strict -- a
;;; trailing comma, a bare word, a number with a leading plus -- because a
;;; language server that mis-parses one message answers the next one wrongly.

(define-record-type <json-input>
  (%make-input text index)
  json-input?
  (text input-text)
  (index input-index set-input-index!))

(define* (json-read text #:key (path "json"))
  "Return the value @var{text} holds.

A syntax error is raised as @code{(throw 'sift-json-error (path line column
message))}, the same shape the configuration reader uses for its own errors,
so that the caller can say where the bad message came from."
  (let ((in (%make-input text 0)))
    (let ((value (read-value in)))
      (skip-space! in)
      (unless (>= (input-index in) (string-length text))
        (fail! in path "trailing text after the value"))
      value)))

(define (read-value in)
  (skip-space! in)
  (let ((c (peek in)))
    (cond
      [(not (char? c)) (fail! in "json" "no value")]
      [(char=? c #\{) (read-object in)]
      [(char=? c #\[) (read-array in)]
      [(char=? c #\") (read-string-literal in)]
      [(or (char=? c #\-) (char-digit? c)) (read-number in)]
      [(char=? c #\t) (read-word in "true" json-true)]
      [(char=? c #\f) (read-word in "false" json-false)]
      [(char=? c #\n) (read-word in "null" json-null)]
      [else (fail! in "json" (format #f "unexpected character ~s" c))])))

(define (char-digit? c)
  (and (char? c) (char-numeric? c)))

(define (peek in)
  "Return the character at the reader, or @code{#f} at the end of the text."
  (let ((i (input-index in)))
    (and (< i (string-length (input-text in)))
      (string-ref (input-text in) i))))

(define (advance! in)
  (set-input-index! in (+ 1 (input-index in))))

(define (skip-space! in)
  (let ((c (peek in)))
    (when (and (char? c) (memv c '(#\space #\tab #\newline #\return)))
      (advance! in)
      (skip-space! in))))

(define (read-word in expected value)
  (let ((start (input-index in)))
    (let loop ((i 0))
      (cond
        [(= i (string-length expected))
         ;; A word that ends the document ends here; one followed by more
         ;; letters is a word JSON has no name for, and is refused rather than
         ;; read as `trueful'.
         (let ((next (peek in)))
           (when (and (char? next) (char-alphabetic? next))
             (fail! in "json" "a bare word is not a JSON value")))
         value]
        [(and (< (input-index in) (string-length (input-text in)))
           (char=? (string-ref (input-text in) (input-index in))
                   (string-ref expected i)))
         (advance! in)
         (loop (+ i 1))]
        [else (fail! in "json" (format #f "expected ~a at ~a" expected start))]))))

(define (read-string-literal in)
  (advance! in) ; the opening quote
  (let loop ((chars '()))
    (let ((c (peek in)))
      (cond
        [(not (char? c)) (fail! in "json" "unterminated string")]
        [(char=? c #\") (advance! in) (list->string (reverse chars))]
        [(char=? c #\\)
         (advance! in)
         (loop (cons (read-escape in) chars))]
        [(char<? c #\space)
         (fail! in "json" "a control character in a string must be escaped")]
        [else (advance! in) (loop (cons c chars))]))))

(define (read-escape in)
  (let ((c (peek in)))
    (advance! in)
    (case c
      [(#\") #\"]
      [(#\\) #\\]
      [(#\/) #\/]
      [(#\b) #\backspace]
      [(#\f) #\page]
      [(#\n) #\newline]
      [(#\r) #\return]
      [(#\t) #\tab]
      [(#\u)
       (let ((code (read-hex4 in)))
         (cond
           ;; A surrogate pair is two \u escapes standing for one character
           ;; above U+FFFF.  A lone surrogate is not a character at all, and
           ;; JSON allows one to appear, so it is replaced rather than raised
           ;; on: the message is unusual enough already.
           [(and (>= code #xd800) (<= code #xdbff))
            (let ((save (input-index in)))
              (advance! in) ; the backslash of the low half
              (if (eqv? (peek in) #\u)
                (let ((low (begin (advance! in) (read-hex4 in))))
                  (if (and (>= low #xdc00) (<= low #xdfff))
                    (integer->char
                     (+ #x10000
                        (* (- code #xd800) #x400)
                        (- low #xdc00)))
                    #\xfffd))
                (begin (set-input-index! in save) #\xfffd)))]
           [(and (>= code #xdc00) (<= code #xdfff)) #\xfffd]
           [else (integer->char code)]))]
      [else (fail! in "json" (format #f "unknown escape \\~a" c))])))

(define (read-hex4 in)
  (let loop ((i 0) (value 0))
    (if (= i 4)
      value
      (let ((c (peek in)))
        (advance! in)
        (let ((digit (hex-digit-value c)))
          (if digit
            (loop (+ i 1) (+ (* value 16) digit))
            (fail! in "json" "\\u needs four hexadecimal digits")))))))

(define (hex-digit-value c)
  (cond
    [(and (char? c) (char-numeric? c)) (- (char->integer c) (char->integer #\0))]
    [(and (char? c) (char-ci>=? c #\a) (char-ci<=? c #\f))
     (+ 10 (- (char->integer (char-downcase c)) (char->integer #\a)))]
    [else #f]))

(define (read-number in)
  (let ((start (input-index in)))
    (when (eqv? (peek in) #\-) (advance! in))
    (when (eqv? (peek in) #\0) (advance! in))
    (unless (eqv? (peek in) #\0)
      (while-digit! in))
    (when (eqv? (peek in) #\.)
      (advance! in)
      (unless (char-digit? (peek in))
        (fail! in "json" "a fraction needs a digit after the point"))
      (while-digit! in))
    (when (memv (peek in) '(#\e #\E))
      (advance! in)
      (when (memv (peek in) '(#\+ #\-)) (advance! in))
      (unless (char-digit? (peek in))
        (fail! in "json" "an exponent needs a digit"))
      (while-digit! in))
    (let ((text (substring (input-text in) start (input-index in))))
      (or (string->number text) (fail! in "json" "not a number")))))

(define (while-digit! in)
  (when (char-digit? (peek in))
    (advance! in)
    (while-digit! in)))

(define (read-array in)
  (advance! in) ; the [
  (skip-space! in)
  (if (eqv? (peek in) #\])
    (begin (advance! in) (vector))
    (let loop ((items '()))
      (let ((value (read-value in)))
        (skip-space! in)
        (let ((c (peek in)))
          (cond
            [(eqv? c #\,)
             (advance! in)
             (loop (cons value items))]
            [(eqv? c #\])
             (advance! in)
             (list->vector (reverse (cons value items)))]
            [else (fail! in "json" "an array needs a comma or a closing bracket")]))))))

(define (read-object in)
  (advance! in) ; the {
  (skip-space! in)
  (if (eqv? (peek in) #\})
    (begin (advance! in) (json-object))
    (let loop ((fields '()))
      (skip-space! in)
      (unless (eqv? (peek in) #\")
        (fail! in "json" "an object member name is a string"))
      (let* ((name (read-string-literal in)))
        (skip-space! in)
        (unless (eqv? (peek in) #\:)
          (fail! in "json" "an object member needs a colon"))
        (advance! in)
        (let ((value (read-value in)))
          (skip-space! in)
          (let ((c (peek in)))
            (cond
              [(eqv? c #\,)
               (advance! in)
               (loop (cons (cons name value) fields))]
              [(eqv? c #\})
               (advance! in)
               (cons json-object-tag (reverse (cons (cons name value) fields)))]
              [else
                (fail! in "json"
                       "an object needs a comma or a closing brace")])))))))

(define (fail! in path message)
  "Raise a JSON error at the reader's position, naming @var{path}."
  (let* ((index (input-index in))
         (text (input-text in))
         (before (substring text 0 (min index (string-length text))))
         (newline (string-rindex before #\newline))
         (line (+ 1 (length (filter (lambda (c) (char=? c #\newline))
                                    (string->list before)))))
         (line-start (if newline (+ newline 1) 0))
         (column (+ 1 (- index line-start))))
    (throw 'sift-json-error (list path line column message))))

;;; ===== Writing =====
;;;
;;; The writer is a fold over the value: a string is written with the escapes
;;; JSON requires, a number with its own spelling, and everything else by the
;;; shape it is.

(define (json-write value)
  "Return @var{value} as a JSON document, without a trailing newline."
  (let ((out (open-output-string)))
    (write-value value out)
    (get-output-string out)))

(define (write-value value out)
  (cond
    [(eq? value json-null) (display "null" out)]
    [(eq? value json-true) (display "true" out)]
    [(eq? value json-false) (display "false" out)]
    [(eq? value #t) (display "true" out)]
    [(eq? value #f) (display "false" out)]
    [(string? value) (write-string value out)]
    [(number? value) (display (json-number->string value) out)]
    [(char? value) (write-string (string value) out)]
    [(symbol? value) (write-string (symbol->string value) out)]
    [(json-object? value)
     (display "{" out)
     (let loop ((fields (cdr value)) (first? #t))
       (unless (null? fields)
         (unless first? (display "," out))
         (write-string (if (symbol? (caar fields))
                         (symbol->string (caar fields))
                         (caar fields))
                       out)
         (display ":" out)
         (write-value (cdar fields) out)
         (loop (cdr fields) #f)))
     (display "}" out)]
    [(vector? value)
     (display "[" out)
     (let loop ((i 0))
       (when (< i (vector-length value))
         (when (> i 0) (display "," out))
         (write-value (vector-ref value i) out)
         (loop (+ i 1))))
     (display "]" out)]
    [(list? value)
     ;; A plain list is an array: a caller writing `(list 1 2)' means the two
     ;; numbers, and the object shape above is the only one that needs a tag.
     (display "[" out)
     (let loop ((items value) (first? #t))
       (unless (null? items)
         (unless first? (display "," out))
         (write-value (car items) out)
         (loop (cdr items) #f)))
     (display "]" out)]
    [else (format out "~s" value)]))

(define (write-string string out)
  (display "\"" out)
  (string-for-each
   (lambda (c)
     (case c
       [(#\") (display "\\\"" out)]
       [(#\\) (display "\\\\" out)]
       [(#\newline) (display "\\n" out)]
       [(#\return) (display "\\r" out)]
       [(#\tab) (display "\\t" out)]
       [(#\backspace) (display "\\b" out)]
       [(#\page) (display "\\f" out)]
       [else
         (if (char<? c #\space)
           (format out "\\u~4,'0x" (char->integer c))
           (display c out))]))
   string)
  (display "\"" out))

(define (json-number->string number)
  "Return @var{number} the way JSON wants to read it back.

An exact integer is written as itself, and anything else through `~f' with
enough digits to be read back as the same number."
  (cond
    [(exact-integer? number) (number->string number)]
    [(and (real? number)
       (let ((as-float (exact->inexact number)))
         (or (not (= as-float as-float)) ; NaN
           (= (abs as-float) (/ 1.0 0.0))))) ; infinity
     "null"]
    [else
      (let ((text (format #f "~f" number)))
        (if (string-find text ".")
          (string-trim-right (string-trim-right text #\0) #\.)
          text))]))
