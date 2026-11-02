;; sift --- `(ice-9 textual-ports)' for a WebAssembly build
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The second Guile library the WebAssembly build has to stand in for.  sift
;; reads and writes *strings*, through ports it makes itself with
;; `open-input-string' and `open-output-string' -- which R7RS has, and Hoot
;; therefore has -- but it names its file reading through
;; `(ice-9 textual-ports)', and a module with an import that does not resolve
;; does not compile at all, whatever the browser would have called.
;;
;; What this answers is what the real library answers, with Guile's export
;; list as the specification: `get-string-all', `get-string-n',
;; `get-string-n!', `get-char', `lookahead-char', `get-line', `put-char' and
;; `put-string'.  Nothing in sift touches the rest of it, so the custom-port
;; constructors and the two `unget' procedures are deliberately absent: an
;; unbound name at compile time says so, which is better than a procedure that
;; pretends.
;;
;; `get-string-all' is the one worth a word.  R7RS has no port-to-string
;; conversion, so it reads a character at a time and joins once at the end.  In
;; the browser there is no file to read -- the facade is handed the whole
;; buffer as a string -- so this exists to make `(sift reader)''s file entry
;; point compile, not to be fast on a megabyte.

(define-library (ice-9 textual-ports)
  (export get-char
    lookahead-char
    get-line
    get-string-all
    get-string-n
    get-string-n!
    put-char
    put-string)
  (import (scheme base)
    (scheme write))

  (begin
    (define (get-char port)
      "Return the next character of @var{port}, or the eof object."
      (read-char port))

    (define (lookahead-char port)
      "Return the next character of @var{port} without consuming it."
      (peek-char port))

    (define (get-line port)
      "Return the next line of @var{port} without its newline, or the eof object."
      (let loop ((chars '()))
        (let ((char (read-char port)))
          (cond
            [(eof-object? char) (if (null? chars) char (list->string (reverse chars)))]
            [(char=? char #\newline) (list->string (reverse chars))]
            [else (loop (cons char chars))]))))

    (define (get-string-all port)
      "Return the rest of @var{port} as a string."
      (let loop ((chars '()))
        (let ((char (read-char port)))
          (if (eof-object? char)
            (list->string (reverse chars))
            (loop (cons char chars))))))

    (define (get-string-n port count)
      "Return at most @var{count} characters of @var{port}, or the eof object."
      (let loop ((i 0) (chars '()))
        (if (>= i count)
          (list->string (reverse chars))
          (let ((char (read-char port)))
            (if (eof-object? char)
              (if (null? chars) char (list->string (reverse chars)))
              (loop (+ i 1) (cons char chars)))))))

    (define (get-string-n! port string start count)
      "Store at most @var{count} characters of @var{port} in @var{string}."
      (let loop ((i 0))
        (if (>= i count)
          i
          (let ((char (read-char port)))
            (if (eof-object? char)
              i
              (begin (string-set! string (+ start i) char)
                (loop (+ i 1))))))))

    (define (put-char port char)
      "Write @var{char} to @var{port}."
      (write-char char port))

    (define (put-string port text . rest)
      "Write @var{text}, or @var{count} characters of it from @var{start}, to @var{port}."
      (let* ((start (if (pair? rest) (car rest) 0))
             (count (if (and (pair? rest) (pair? (cdr rest))) (cadr rest) #f))
             (end (if count (min (string-length text) (+ start count))
                    (string-length text))))
        (when (< start end)
          (display (substring text start end) port))))))
