;; sift --- `(ice-9 format)' for a WebAssembly build
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; sift writes every message it has with `format', and `(ice-9 format)' is the
;; one Guile library the WebAssembly build does not have: Hoot ships an R7RS
;; `(scheme write)', `(ice-9 match)', `(srfi srfi-1)' and `(srfi srfi-13)', but
;; the pretty-printer's directive language is not among them -- and it could
;; not be borrowed anyway, because Guile's is written against unbuffered Guile
;; ports and the whole of `(guile)'.
;;
;; So this file is the subset sift actually uses, and no more.  It is put at
;; the front of the module load path when compiling for the browser (see
;; web/build.sh), so `(ice-9 format)' in sift's sources resolves to it and not
;; one line of src/sift changes to run where there is no Guile.
;;
;; Supported:
;;
;;   ~a   display        ~s   write            ~c   a character
;;   ~d   decimal        ~x   hexadecimal      ~o   octal         ~b   binary
;;   ~f   a real number  ~%   newline          ~&   newline       ~~   a tilde
;;
;; each with the `~mincol,colinc,minpad,padchar' parameter form: the first
;; parameter pads the field to a column, the fourth names the character it pads
;; with.  `~4,'0x' -- hexadecimal in a field of four, zero-padded -- is the one
;; sift writes itself, in the JSON writer's escape for a control character.
;; The `:' and `@' modifiers are consumed and otherwise ignored.
;;
;; Not supported, and refused rather than misprinted: the iteration directives
;; (`~{ ~}'), indirection (`~?'), column layout (`~t') and pretty-printing.
;; Nothing in sift asks for them, and a browser build that silently printed
;; something else would be worse than one that says it cannot.

(define-library (ice-9 format)
  (export format)
  (import (scheme base)
    (scheme char)
    (scheme write))

  (begin
    (define (digit? c)
      (char<=? #\0 c #\9))

    (define (param-char? c)
      (or (digit? c) (char=? c #\-) (char=? c #\')))

    (define (finish-param raw)
      "Return the value of the parameter written RAWS, or #f when it was empty."
      (cond
        [(string=? raw "") #f]
        [(char=? (string-ref raw 0) #\') (string-ref raw 1)]
        [else (string->number raw)]))

    (define (scan-param template i)
      "Return the raw text of the parameter at I, and the index after it."
      (let loop ((i i) (chars '()))
        (cond
          [(>= i (string-length template)) (values (list->string (reverse chars)) i)]
          [(char=? (string-ref template i) #\')
           ;; a quoted character is a parameter to itself: `'0'
           (loop (+ i 2) (cons (string-ref template (+ i 1)) (cons #\q chars)))]
          [(param-char? (string-ref template i))
           (loop (+ i 1) (cons (string-ref template i) chars))]
          [else (values (list->string (reverse chars)) i)])))

    (define (scan-params template i)
      "Return the parameters of the directive at I, and the index after them."
      (let loop ((i i) (params '()))
        (call-with-values (lambda () (scan-param template i))
          (lambda (raw next)
            (let ((params (cons (finish-param raw) params)))
              (if (and (< next (string-length template))
                    (char=? (string-ref template next) #\,))
                (loop (+ next 1) params)
                (values (reverse params) next)))))))

    (define (param params i default)
      "Return the I-th parameter, or DEFAULT when there is no such parameter."
      (if (< i (length params))
        (let ((value (list-ref params i)))
          (if value value default))
        default))

    (define (rendered thunk)
      "Return what THUNK writes, as a string."
      (let ((port (open-output-string)))
        (thunk port)
        (get-output-string port)))

    (define (pad text mincol padchar left?)
      "Return TEXT padded with PADCHAR to MINCOL columns."
      (if (or (not mincol) (<= (string-length text) mincol))
        text
        (let ((filler (make-string (- mincol (string-length text)) padchar)))
          (if left? (string-append filler text) (string-append text filler)))))

    (define (radix-of directive)
      (case directive
        [(#\x) 16]
        [(#\o) 8]
        [(#\b) 2]
        [else 10]))

    (define (integer-text number directive)
      "Return NUMBER written in the radix DIRECTIVE names, in lower case."
      (let ((exact-number (if (exact? number) number (exact (truncate number)))))
        (list->string
         (map char-downcase
              (string->list (number->string exact-number (radix-of directive)))))))

    ;; `~@[...~]' is a conditional: its body is a template, and a template is
    ;; what `emit-template' walks, so the body is walked by the same loop.

    (define (conditional-body template i)
      "Return the body of the `~@[...~]' opened before @var{i}, and the index
after the `~]' that closes it."
      (let loop ((j i) (depth 1))
        (cond
          [(>= j (string-length template))
           (error "format: a ~@[...~] is never closed in" template)]
          [(and (char=? (string-ref template j) #\~)
             (< (+ j 1) (string-length template))
             (char=? (string-ref template (+ j 1)) #\[))
           (loop (+ j 2) (+ depth 1))]
          [(and (char=? (string-ref template j) #\~)
             (< (+ j 1) (string-length template))
             (char=? (string-ref template (+ j 1)) #\]))
           (if (= depth 1)
             (values (substring template i j) (+ j 2))
             (loop (+ j 2) (- depth 1)))]
          [else (loop (+ j 1) depth)])))

    (define (directive-text template i args)
      "Return the text of the directive at I, the index after it, and the rest of ARGS."
      (define (take-arg args)
        (if (pair? args)
          (car args)
          (error "format: not enough arguments for" template)))
      (call-with-values (lambda () (scan-params template (+ i 1)))
        (lambda (params j)
          (let* ((mincol (param params 0 #f))
                 ;; A radix directive takes its padding character second --
                 ;; `~4,'0x' is hexadecimal in a field of four, zero-padded --
                 ;; while `~a' and `~s' take it fourth.  That is the one
                 ;; difference between the two families, and it is why
                 ;; `~4,'0x' in sift's JSON writer prints `00ff'.
                 (numeric-padchar (param params 1 #\space))
                 (padchar (param params 3 #\space))
                 ;; `:' and `@' may follow the parameters.  `:' is consumed and
                 ;; nothing else; `@' is the one modifier sift writes -- in
                 ;; `~@[...~]' -- so it is remembered rather than dropped.
                 (at? #f)
                 (j (let skip ((j j))
                      (cond
                        [(and (< j (string-length template))
                           (char=? (string-ref template j) #\@))
                         (set! at? #t)
                         (skip (+ j 1))]
                        [(and (< j (string-length template))
                           (char=? (string-ref template j) #\:))
                         (skip (+ j 1))]
                        [else j]))))
            (if (>= j (string-length template))
              (error "format: the directive ends the format string at" i)
              (let ((directive (string-ref template j))
                    (rest (if (pair? args) (cdr args) '())))
                (case directive
                  [(#\a)
                   (values (pad (rendered (lambda (port)
                                            (display (take-arg args) port)))
                                mincol padchar #f)
                           (+ j 1) rest)]
                  [(#\s)
                   (values (pad (rendered (lambda (port)
                                            (write (take-arg args) port)))
                                mincol padchar #f)
                           (+ j 1) rest)]
                  [(#\d #\x #\o #\b)
                   (values (pad (integer-text (take-arg args) directive)
                                mincol numeric-padchar #t)
                           (+ j 1) rest)]
                  [(#\f)
                   (values (pad (number->string (take-arg args))
                                mincol numeric-padchar #t)
                           (+ j 1) rest)]
                  [(#\c)
                   (values (pad (string (take-arg args)) mincol padchar #f)
                           (+ j 1) rest)]
                  [(#\%)
                   (values "\n" (+ j 1) args)]
                  [(#\&)
                   (values "\n" (+ j 1) args)]
                  [(#\~)
                   (values "~" (+ j 1) args)]
                  [(#\[)
                   (if (not at?)
                     (error "format: only the conditional ~@[...~] is supported, of"
                            template)
                     ;; Process the body when the next argument is true, and
                     ;; consume that argument either way.  The body is walked
                     ;; with the arguments as they are, so its own directives
                     ;; see the same one -- which is what makes
                     ;; `(format #f "x~@[ ~a~]" 5)' print "x 5".
                     (call-with-values (lambda () (conditional-body template (+ j 1)))
                       (lambda (body next)
                         (let ((port (open-output-string)))
                           (if (and (pair? args) (car args))
                             (call-with-values (lambda () (emit-template body args port))
                               (lambda (left)
                                 (values (get-output-string port) next left)))
                             (values "" next (if (pair? args) (cdr args) args)))))))]
                  [else
                    (error "format: this build does not support the directive"
                           (string directive)
                           "of" template)])))))))

    (define (emit-template template args port)
      "Write @var{template} to @var{port} and return the arguments left over."
      (let loop ((i 0) (args args))
        (cond
          [(>= i (string-length template)) args]
          [(char=? (string-ref template i) #\~)
           (call-with-values (lambda () (directive-text template i args))
             (lambda (text next rest)
               (display text port)
               (loop next rest)))]
          [else
            (display (string-ref template i) port)
            (loop (+ i 1) args)])))

    (define (format destination template . args)
      "Return a string, or write to a port, the way `(ice-9 format)' does.

With @var{destination} #f the answer is a string; with #t it is written to the
current output port; with a port it is written there."
      (let ((to-string? (not destination))
            (port (cond [(not destination) (open-output-string)]
                        [(eq? destination #t) (current-output-port)]
                        [else destination])))
        (emit-template template args port)
        (when to-string? (get-output-string port))))))
