;; sift --- reader
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A hand-written, dialect-aware, location-preserving reader.
;;
;; Guile's `read' is not used anywhere in sift, and cannot be: it drops
;; comments, keeps no locations, and has no way to represent a block comment,
;; a datum comment or a reader prefix as a node of its own.  Everything the
;; formatter must put back -- every comment, every blank line, every "'" --
;; is therefore read here into a node that carries its source span.
;;
;; The reader is permissive.  A malformed file produces nodes for the part it
;; could read plus a list of parse errors; it never aborts a file because one
;; form in it is broken, and it never guesses at a missing delimiter.
;;
;; Whitespace is read as nodes too, and split in two: a run of spaces and tabs
;; becomes one whitespace node, and a run of newlines becomes another one
;; carrying the count.  That split is what lets the formatter preserve blank
;; lines and decide which comments are inline.

(define-module (sift reader)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (sift strings)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (sift records)
  #:re-export (node->string)
  #:export (read-source
            read-source-string
            read-source/file
            ast->text
            detect-dialect
            dialect-marker
            string->ast-list
            source-lines
            line-count
            line-of
            source-line))

;;; ===== Reader state =====

(define-record-type <reader>
  (%make-reader source file dialect length line-starts byte-offsets index errors)
  reader?
  (source reader-source)
  (file reader-file)
  (dialect reader-dialect)
  (length reader-length)
  (line-starts reader-line-starts)
  (byte-offsets reader-byte-offsets)
  (index reader-index set-reader-index!)
  (errors reader-errors set-reader-errors!))

(define* (make-reader source #:key (file #f) (dialect 'scheme))
  "Return a reader positioned at the beginning of @var{source}."
  (let ((starts (compute-line-starts source))
        (bytes (compute-byte-offsets source)))
    (%make-reader source file dialect (string-length source)
                  starts bytes 0 '())))

;; --- source indexing ---------------------------------------------------

(define (compute-line-starts source)
  "Return a vector of the character indices at which lines begin.

A line break is a newline, or a carriage return that is not followed by
one, so a file in CRLF style has the same number of lines as the same file
after a `dos2unix'."
  (let* ((n (string-length source))
         (starts (let loop ((i 0) (acc '(0)))
                   (cond
                     [(>= i n) (reverse acc)]
                     [(char=? (string-ref source i) #\newline)
                      (loop (+ i 1) (cons (+ i 1) acc))]
                     [(and (char=? (string-ref source i) #\return)
                        (or (>= (+ i 1) n)
                          (not (char=? (string-ref source (+ i 1))
                                       #\newline))))
                      (loop (+ i 1) (cons (+ i 1) acc))]
                     [else (loop (+ i 1) acc)]))))
    (list->vector starts)))

(define (line-count source)
  "Return the number of lines in @var{source}; a file of one line returns 1."
  (let ((n (string-length source)))
    (if (= n 0)
      1
      (let loop ((i 0) (count 1))
        (if (>= i n)
          count
          (let ((c (string-ref source i)))
            (cond
              [(char=? c #\newline) (loop (+ i 1) (+ count 1))]
              [(and (char=? c #\return)
                 (or (>= (+ i 1) n)
                   (not (char=? (string-ref source (+ i 1))
                                #\newline))))
               (loop (+ i 1) (+ count 1))]
              [else (loop (+ i 1) count)])))))))

(define (source-lines source)
  "Return @var{source} split into lines, without their line breaks."
  (let* ((text (if (string-index source #\return)
                 (normalise-newlines source)
                 source))
         (n (string-length text)))
    (if (= n 0)
      (list "")
      (let loop ((start 0) (i 0) (out '()))
        (cond
          [(>= i n)
           (reverse (cons (substring text start i) out))]
          [(char=? (string-ref text i) #\newline)
           (loop (+ i 1) (+ i 1) (cons (substring text start i) out))]
          [else (loop start (+ i 1) out)])))))

(define (normalise-newlines source)
  "Return @var{source} with every CRLF and lone CR turned into a newline."
  (call-with-output-string
    (lambda (out)
      (let ((n (string-length source)))
        (let loop ((i 0))
          (when (< i n)
            (let ((c (string-ref source i)))
              (cond
                [(char=? c #\return)
                 (display #\newline out)
                 (if (and (< (+ i 1) n)
                       (char=? (string-ref source (+ i 1)) #\newline))
                   (loop (+ i 2))
                   (loop (+ i 1)))]
                [else (display c out) (loop (+ i 1))]))))))))

(define (line-of source index)
  "Return the 1-based line number of @var{index} in @var{source}."
  (let* ((starts (compute-line-starts source))
         (limit index))
    (let loop ((i 0))
      (cond
        [(>= (+ i 1) (vector-length starts)) (+ i 1)]
        [(<= (vector-ref starts (+ i 1)) limit) (loop (+ i 1))]
        [else (+ i 1)]))))

(define (source-line source line)
  "Return line @var{line} (1-based) of @var{source}, without its break."
  (let ((lines (source-lines source)))
    (and (>= line 1) (<= line (length lines))
      (list-ref lines (- line 1)))))

(define (compute-byte-offsets source)
  "Return a vector mapping character indices to byte offsets.

`source-location-offset' is a byte offset because that is what an editor and
a patch expect; the reader works in character indices because that is what
`substring' counts.  This vector keeps the two from ever drifting, and it is
built once per file so the conversion costs nothing per node."
  (let* ((n (string-length source))
         (offsets (make-vector (+ n 1) 0)))
    (vector-set! offsets 0 0)
    (let loop ((i 0) (bytes 0))
      (if (>= i n)
        offsets
        (let* ((c (string-ref source i))
               (width (if (char<? c #\x80)
                        1
                        (bytevector-length (string->utf8 (string c))))))
          (vector-set! offsets (+ i 1) (+ bytes width))
          (loop (+ i 1) (+ bytes width)))))))

;; --- position helpers --------------------------------------------------

(define (at-end? r)
  (>= (reader-index r) (reader-length r)))

(define (peek r)
  (if (at-end? r) #f (string-ref (reader-source r) (reader-index r))))

(define (peek-at r k)
  (let ((i (+ (reader-index r) k)))
    (if (< i (reader-length r)) (string-ref (reader-source r) i) #f)))

(define (looking-at? r text)
  "Return @code{#t} when the source at @var{r}'s index begins with @var{text}."
  (let loop ((i 0))
    (cond
      [(>= i (string-length text)) #t]
      [else
        (let ((c (peek-at r i)))
          (and c (char=? c (string-ref text i)) (loop (+ i 1))))])))

(define (advance! r)
  (set-reader-index! r (+ 1 (reader-index r))))

(define (advance-by! r k)
  (set-reader-index! r (+ k (reader-index r))))

(define (index->location r index)
  "Return the <source-location> of character @var{index} in @var{r}'s source."
  (let* ((starts (reader-line-starts r))
         (line (line-index starts index))
         (line-start (vector-ref starts line)))
    (make-source-location* (reader-file r)
                           (+ line 1)
                           (+ 1 (- index line-start))
                           (vector-ref (reader-byte-offsets r) index)
                           index)))

(define (line-index starts index)
  "Return the 0-based line of @var{index}, given the vector of line starts."
  (let loop ((lo 0) (hi (- (vector-length starts) 1)))
    (if (>= lo hi)
      lo
      (let ((mid (quotient (+ lo hi 1) 2)))
        (if (<= (vector-ref starts mid) index)
          (loop mid hi)
          (loop lo (- mid 1)))))))

(define (record-error! r index severity message)
  "Record a parse problem at character @var{index} in @var{r}."
  (set-reader-errors!
   r (cons (make-lint-warning 'reader severity message
                              (index->location r index))
           (reader-errors r))))

;; --- character classes ------------------------------------------------

(define (whitespace-char? c)
  (and c (memv c '(#\space #\tab #\x0c #\x0b))))

(define (newline-char? c)
  (and c (memv c '(#\newline #\return))))

(define (space-or-newline? c)
  "Return @code{#t} when @var{c} is whitespace of any kind."
  (and c (or (whitespace-char? c) (newline-char? c))))

(define (dialect-delimiters dialect)
  "Return the characters that end an atom in @var{dialect}, whitespace aside.

A delimiter is what a reader stops at, and which characters they are is the
dialect's business.  The differences are not academic:

- In Scheme a quote, a backquote and a comma belong to a symbol -- `number''
  and `lst'' are the names Guile's own sources give their variables, and every
  R7RS reader agrees -- while Emacs Lisp and Common Lisp end the symbol there.
- Emacs Lisp ends a symbol at a `#' as well; Common Lisp does not, and its
  symbols may hold a `[' or a `#' in the middle (`A#B', `A[B').
- Nothing ends an atom at a brace in any of the four: `a{b' is one name in
  Guile, in Emacs Lisp and in Common Lisp, and so is `{a'.  Racket writes a
  name with a delimiter in it as `|a b|', and its backslash is not an escape
  inside a name either."
  (case dialect
    [(elisp) '(#\( #\) #\[ #\] #\" #\; #\' #\` #\, #\#)]
    [(cl) '(#\( #\) #\" #\; #\' #\` #\,)]
    [else '(#\( #\) #\[ #\] #\" #\;)]))

(define (atom-delimiter? r c)
  "Return @code{#t} when @var{c} ends an atom in the dialect @var{r} reads.

A `#' is not a delimiter in Scheme or in Common Lisp: it only starts a reader
directive at the beginning of a token, and a symbol such as `foo#bar' (Scheme)
or `A#B' (Common Lisp) is one atom.  A quote is not one in Scheme either: what
Guile calls `x'' is a single name, not a name and a quote."
  (and c
    (or (space-or-newline? c)
      (memv c (dialect-delimiters (reader-dialect r))))))

;;; ===== Node constructors =====

(define (whitespace-node r start-index end-index newlines)
  (make-node 'whitespace
             (if (> newlines 0)
               (make-string newlines #\newline)
               (substring (reader-source r) start-index end-index))
             (index->location r start-index)
             (index->location r end-index)
             (list (cons 'newlines newlines))))

(define (comment-node r start-index end-index inline?)
  (make-node 'comment
             (substring (reader-source r) start-index end-index)
             (index->location r start-index)
             (index->location r end-index)
             (list (cons 'inline inline?))))

(define* (span-node r kind start end #:optional (fields '()))
  "Return a node of @var{kind} holding the source text from @var{start} to @var{end}.

A string literal, a block comment, an unreadable object: each is copied out
of the source as it was written and nothing else, so the node that stands
for one is nothing but its kind, its span and whatever the kind needs on
top.  The reader has already advanced past @var{end} when it asks for one."
  (make-node kind (substring (reader-source r) start end)
             (index->location r start) (index->location r end) fields))

(define (literal-node r start end atom-type)
  "Return an atom of @var{atom-type} for the source text from @var{start} to @var{end}.

A character literal or a `|bar-quoted|' symbol is an atom whose value is the
text the author wrote -- never respelled, never classified further."
  (span-node r 'atom start end (list (cons 'atom-type atom-type))))

(define (dialect-name dialect)
  "Return the name of @var{dialect} as a diagnostic spells it.

The reader is permissive by design: it reads a construct that belongs to
another dialect anyway, keeps every byte of it, and says so in the dialect's
name, so that a file which mixed two dialects is reported where it happens
rather than formatted into something neither dialect reads."
  (case dialect
    [(racket) "Racket"]
    [(elisp) "Emacs Lisp"]
    [(cl) "Common Lisp"]
    [else "Scheme"]))

(define (scheme-family? dialect)
  "Return @code{#t} when @var{dialect} reads the syntax the Scheme reports define.

Racket is a Scheme here and a language of its own everywhere else: `#;' and
`#|...|#', a bytevector, a square bracket, `#'`/`#``/`#,` and a `#\\a' are all
Racket too, while a Guile `#{...}#' symbol, a gexp prefix and a `\\' escape
inside a name are not."
  (memq dialect '(scheme racket)))

;;; ===== Reading trivia =====

(define (read-newline-run r)
  (let ((start (reader-index r)))
    (let loop ((count 0))
      (let ((c (peek r)))
        (cond
          [(eqv? c #\newline) (advance! r) (loop (+ count 1))]
          [(eqv? c #\return)
           (advance! r)
           (when (eqv? (peek r) #\newline) (advance! r))
           (loop (+ count 1))]
          [else (whitespace-node r start (reader-index r) count)])))))

(define (read-space-run r)
  (let ((start (reader-index r)))
    (while (whitespace-char? (peek r)) (advance! r))
    (whitespace-node r start (reader-index r) 0)))

(define (line-prefix-blank? r index)
  "Return @code{#t} when only whitespace precedes @var{index} on its line."
  (let* ((starts (reader-line-starts r))
         (line (line-index starts index))
         (from (vector-ref starts line)))
    (let loop ((i from))
      (cond
        [(>= i index) #t]
        [(whitespace-char? (string-ref (reader-source r) i)) (loop (+ i 1))]
        [(newline-char? (string-ref (reader-source r) i)) (loop (+ i 1))]
        [else #f]))))

(define (read-comment r)
  (let* ((start (reader-index r))
         (inline? (not (line-prefix-blank? r start))))
    (while (and (peek r) (not (newline-char? (peek r)))) (advance! r))
    (comment-node r start (reader-index r) inline?)))

(define (read-shebang r)
  (let ((start (reader-index r)))
    (while (and (peek r) (not (newline-char? (peek r)))) (advance! r))
    (span-node r 'shebang start (reader-index r))))

(define (read-language-line r)
  "Read the `#lang' line a Racket module opens with, as one node.

`#lang racket' is not a datum, a comment or a shebang: it is the form that says
what the file is, and a reader that split it into two atoms would be reading a
module header as a call with no operator.  The line is kept whole, verbatim,
and it is data rather than trivia -- the formatter separates it from the
`require' below it the way every Racket file does."
  (let ((start (reader-index r)))
    (unless (eq? (reader-dialect r) 'racket)
      (record-error! r start 'warning
                     (format #f "a `#lang' line is Racket syntax, not ~a syntax"
                             (dialect-name (reader-dialect r)))))
    (while (and (peek r) (not (newline-char? (peek r)))) (advance! r))
    (span-node r 'language start (reader-index r))))

;;; ===== Reading bracket forms =====

(define (read-bytevector r open-text)
  "Read a bytevector literal written @var{open-text} -- `#u8(' or `#vu8('.

A bytevector is bracketed like a list and holds numbers rather than
s-expressions, which is a rule for the checkers and the formatter, not for
the reader: the reader reads it as a bracketed datum whose opener is the
whole token, and every element inside it is read exactly as it would be
anywhere else."
  (unless (scheme-family? (reader-dialect r))
    (record-error! r (reader-index r) 'warning
                   (format #f "bytevectors are not ~a syntax"
                           (dialect-name (reader-dialect r)))))
  (read-balanced r open-text))

(define (matching-close open)
  (case open
    [(#\() #\)]
    [(#\[) #\]]
    [else #\)]))

(define (bracket-fields r open-text close)
  "Return the fields of a bracketed node: its delimiters, and its kind.

The reader is the only place that knows the dialect, so it is the place that
answers \"is this `[' a vector or a parenthesis by another name?\": in Emacs
Lisp a square bracket holds a vector, and in Scheme and Common Lisp it is a
list written with square brackets.  Recording the answer on the node is what
lets `node-vector?' answer correctly without being handed a dialect."
  (append
   (list (cons 'open open-text) (cons 'close close))
   (cond
     [(string=? open-text "#(") (list (cons 'vector #t))]
     [(and (eq? (reader-dialect r) 'elisp) (string=? open-text "["))
      (list (cons 'vector #t))]
     [else '()])))

(define* (read-balanced r open-text #:optional (open-char #f))
  "Read a bracketed form whose opening text is @var{open-text}.

@var{open-char} is the character that closes it -- `#' for a list -- and the
closing character of @var{open-text} is used when it is not given.  A form
that runs to end of file records a parse error at its opener and is returned
without a closer: the reader never invents a `)'."
  (let* ((start (reader-index r))
         (close-char (matching-close (or open-char
                                       (string-ref open-text
                                                   (- (string-length open-text)
                                                      1))))))
    (advance-by! r (string-length open-text))
    (let loop ((children '()))
      (cond
        [(at-end? r)
         (record-error! r start 'error
                        "unterminated form: the file ends before this form is closed")
         (make-node 'list (reverse children)
                    (index->location r start)
                    (index->location r (reader-index r))
                    (bracket-fields r open-text #f))]
        [(eqv? (peek r) close-char)
         (advance! r)
         (make-node 'list (reverse children)
                    (index->location r start)
                    (index->location r (reader-index r))
                    (bracket-fields r open-text (string close-char)))]
        [else
          (let ((node (read-node r)))
            (if (eq? node '*eof*)
              (loop children)
              (loop (cons node children))))]))))

(define (read-stray-close r)
  (let ((start (reader-index r))
        (c (peek r)))
    (advance! r)
    (record-error! r start 'error
                   (format #f "unmatched closing delimiter `~a'" c))
    (make-node 'atom (string c)
               (index->location r start)
               (index->location r (reader-index r))
               (list (cons 'atom-type 'stray-delimiter)))))

;;; ===== Strings and characters =====

(define (read-string r)
  (let ((start (reader-index r)))
    (advance! r) ; the opening quote
    (let loop ()
      (cond
        [(at-end? r)
         (record-error! r start 'error "unterminated string")
         (literal-node r start (reader-index r) 'string)]
        [(eqv? (peek r) #\")
         (advance! r)
         (literal-node r start (reader-index r) 'string)]
        [(eqv? (peek r) #\\)
         ;; An escape is copied as it was written.  Nothing here decodes a
         ;; string, because the formatter must never respell an escape.
         (advance! r)
         (unless (at-end? r) (advance! r))
         (loop)]
        [else (advance! r) (loop)]))))

(define (hex-digit? c)
  "Return @code{#t} when @var{c} is one of the digits of a hexadecimal number."
  (and c (memv c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                   #\a #\b #\c #\d #\e #\f
                   #\A #\B #\C #\D #\E #\F)) #t))

(define (read-scheme-char-literal r)
  "Read a Scheme character literal: #\\a, #\\space, #\\x41."
  (let ((start (reader-index r)))
    (advance-by! r 2) ; # and \
    (let ((c (peek r)))
      (cond
        [(not c) #f]
        [(and (memv c '(#\x #\X)) (hex-digit? (peek-at r 1)))
         (advance! r)
         (while (hex-digit? (peek r)) (advance! r))]
        [(char-alphabetic? c)
         (while (let ((d (peek r)))
                  (and d (or (char-alphabetic? d)
                           (and (memv d '(#\- #\_))
                             (char-alphabetic? (peek-at r 1))))))
           (advance! r))]
        [else (advance! r)])
      (literal-node r start (reader-index r) 'character))))

(define (read-elisp-char-literal r)
  "Read an Emacs Lisp character literal: ?a, ?\\n, ?\\C-x, ?\\M-\\C-x, ?\\s.

The syntax is a `?' followed by either one character or a backslash escape,
which may carry the case and meta modifiers the manual documents.  The text
is kept exactly as it was written."
  (let ((start (reader-index r)))
    (advance! r) ; the ?
    (if (eqv? (peek r) #\\)
      (begin
        (advance! r) ; the backslash
        ;; case and meta modifiers, as in ?\\C-x and ?\\M-\\C-x
        (let loop ()
          (let ((c (peek r)))
            (when (and c
                    (memv c '(#\C #\M #\S #\H #\A #\s))
                    (eqv? (peek-at r 1) #\-))
              (advance-by! r 2)
              (when (eqv? (peek r) #\\) (advance! r))
              (loop))))
        (let ((c (peek r)))
          (cond
            [(not c) #f]
            [(eqv? c #\^)
             ;; `?\^I' is control-I: a `^' and the character it applies to.
             ;; Every other escape is one character -- `?\\' is a backslash
             ;; and ends there -- so consuming two would eat the delimiter
             ;; after it and leave the form unterminated.
             (advance! r)
             (unless (at-end? r) (advance! r))]
            [else (advance! r)])))
      ;; a plain character literal: ?a, ?(, ?\s
      (unless (at-end? r) (advance! r)))
    (literal-node r start (reader-index r) 'character)))

;;; ===== Atoms =====

(define (bar-symbol-dialect? r)
  "Return @code{#t} when @code{|...|} quotes a symbol in @var{r}'s dialect.

Scheme reads `|foo bar|' as the symbol `foo bar', and Common Lisp reads it as
a name whose characters are taken literally.  Emacs Lisp does not: a `|' there
is an ordinary symbol character, so `(rx (| a b))' is a call to a symbol named
`|' and must not be read as an escaped name that runs to the next bar."
  (not (eq? (reader-dialect r) 'elisp)))

(define (read-bar-symbol r)
  "Read a |bar-quoted symbol|, escapes included, verbatim."
  (let ((start (reader-index r)))
    (advance! r)
    (let loop ()
      (cond
        [(at-end? r)
         (record-error! r start 'error "unterminated |symbol|")
         (literal-node r start (reader-index r) 'symbol)]
        [(eqv? (peek r) #\\) (advance! r) (unless (at-end? r) (advance! r)) (loop)]
        [(eqv? (peek r) #\|) (advance! r)
         (literal-node r start (reader-index r) 'symbol)]
        [else (advance! r) (loop)]))))

(define (read-extended-symbol r)
  "Read Guile's @code{#{...}#} symbol, every character of it, verbatim.

The syntax exists for the symbols code that writes code can produce and nobody
wants to spell: a name with a space, a newline or a bracket in it.  It does not
nest: the first `}#` that is not escaped ends the name, so a backslash is
honoured only to the extent that it keeps a `}` from closing the symbol, and
everything between the two markers is kept exactly as written."
  (let ((start (reader-index r)))
    (unless (eq? (reader-dialect r) 'scheme)
      (record-error! r start 'warning
                     "extended symbols like `#{}#' are Guile syntax"))
    (advance-by! r 2) ; # and {
    (let loop ()
      (cond
        [(at-end? r)
         (record-error! r start 'error "unterminated #{...}# symbol")
         (literal-node r start (reader-index r) 'symbol)]
        [(eqv? (peek r) #\\)
         (advance! r)
         (unless (at-end? r) (advance! r))
         (loop)]
        [(and (eqv? (peek r) #\}) (eqv? (peek-at r 1) #\#))
         (advance-by! r 2)
         (literal-node r start (reader-index r) 'symbol)]
        [else (advance! r) (loop)]))))

(define (symbol-escape? r)
  "Return @code{#t} when a backslash quotes the next character of a symbol.

Emacs Lisp and Common Lisp both use a single escape inside a symbol, so `\\('
is the symbol `(' and `\\#' is the symbol `#' -- neither opens a list nor
starts a reader directive.  Scheme has no such escape: a backslash in a symbol
is a stray character there, and Guile does not read one; Racket quotes a name
with `|...|' instead, so it has none either."
  (memq (reader-dialect r) '(elisp cl)))

(define (read-atom r)
  "Read an atom, honouring whatever the dialect makes of an escape.

The escape is kept as it was written, like every other byte the reader copies:
what it decides is only where the atom ends, which is what a formatter has to
get right and what a symbol named `\\(' depends on."
  (let ((start (reader-index r)))
    (let loop ()
      (let ((c (peek r)))
        (cond
          [(not c) #f]
          [(and (char=? c #\\) (symbol-escape? r))
           (advance! r)
           (unless (at-end? r) (advance! r))
           (loop)]
          [(atom-delimiter? r c) #f]
          [else (advance! r) (loop)])))
    (when (= start (reader-index r)) ; a lone delimiter we do not know
      (advance! r))
    (atom-node r start (reader-index r))))

(define (atom-node r start end)
  "Classify the text from @var{start} to @var{end} and return its node."
  (let* ((text (substring (reader-source r) start end))
         (number (and (not (string-prefix? "|" text))
                   (string->number text)))
         (type (cond
                 [number 'number]
                 [(member text '("#t" "#f" "#true" "#false")) 'boolean]
                 ;; Racket spells a keyword `#:height', and it is a keyword
                 ;; there rather than the uninterned symbol `#:height' Common
                 ;; Lisp writes: the two differ only in the dialect that read
                 ;; them, so the reader says which one it was.
                 [(and (eq? (reader-dialect r) 'racket)
                    (string-prefix? "#:" text))
                  'keyword]
                 [(and (> (string-length text) 1)
                    (char=? (string-ref text 0) #\:))
                  'keyword]
                 [else 'symbol]))
         (marker (and (eq? type 'symbol) (package-marker-of text))))
    (make-node 'atom text
               (index->location r start)
               (index->location r end)
               (append (list (cons 'atom-type type)
                             (cons 'value number))
                       (if marker (list (cons 'package-marker marker)) '())))))

(define (package-marker-of text)
  "Return the package marker of a Common Lisp symbol name, or @code{#f}.

`foo:bar' has the marker \"foo:\" and `foo::bar' the marker \"foo::\"; both
are distinct spellings of a symbol and the reader keeps them apart.  An
uninterned symbol `#:foo' carries the marker \"#:\"."
  (cond
    [(string-prefix? "#:" text) "#:"]
    [(string-index text #\:)
     (let ((at (string-index text #\:)))
       (when (> at 0)
         (if (and (< (+ at 1) (string-length text))
               (char=? (string-ref text (+ at 1)) #\:))
           (substring text 0 (+ at 2))
           (substring text 0 (+ at 1)))))]
    [else #f]))

;;; ===== Reader prefixes and directives =====

(define (read-prefix r token)
  "Read the prefix @var{token} and the datum it applies to, as one node.

Whatever whitespace stood between the two is recorded in the node's `gap'
field: the formatter closes the gap, and the `reader-prefix-break' checker
is what reports it."
  (let* ((start (reader-index r))
         (gap-start (begin (advance-by! r (string-length token))
                      (reader-index r))))
    (let loop ()
      (when (space-or-newline? (peek r))
        (advance! r)
        (loop)))
    (let ((gap (substring (reader-source r) gap-start (reader-index r))))
      (let ((child-start (reader-index r))
            (child (read-node r)))
        (cond
          [(or (eq? child '*eof*)
             (and child (node-trivia? child)))
           ;; A comment between a prefix and its datum cannot be folded into
           ;; the prefix without losing it, so the prefix ends here: the
           ;; comment is left for the caller to read as the next node.
           (record-error! r start 'error
                          (format #f "reader prefix `~a' is not followed by a datum"
                                  token))
           (set-reader-index! r child-start)
           (make-node 'prefix token
                      (index->location r start)
                      (index->location r child-start)
                      (list (cons 'child #f)
                            (cons 'gap "")))]
          [else
            (make-node 'prefix token
                       (index->location r start)
                       (index->location r (child-end child))
                       (list (cons 'child child)
                             (cons 'gap gap)))])))))

(define (child-end node)
  "Return the character index just past @var{node}."
  (source-location-index (node-end node)))

(define (read-block-comment r)
  (let ((start (reader-index r)))
    (when (eq? (reader-dialect r) 'elisp)
      (record-error! r start 'warning
                     (format #f "block comments are not ~a syntax"
                             (dialect-name (reader-dialect r)))))
    (advance-by! r 2)
    (let loop ((depth 1))
      (cond
        [(at-end? r)
         (record-error! r start 'error "unterminated block comment")
         (span-node r 'block-comment start (reader-index r))]
        [(and (eqv? (peek r) #\|) (eqv? (peek-at r 1) #\#))
         (advance-by! r 2)
         (if (= depth 1)
           (span-node r 'block-comment start (reader-index r))
           (loop (- depth 1)))]
        [(and (eqv? (peek r) #\#) (eqv? (peek-at r 1) #\|))
         (advance-by! r 2)
         (loop (+ depth 1))]
        [else (advance! r) (loop depth)]))))

(define (mark-commented-out node)
  "Return @var{node} with every node under it flagged as commented out."
  (define (walk node)
    (cond
      [(node-list? node)
       (make-node 'list (map walk (node-children node))
                  (node-start node) (node-end node)
                  (cons (cons 'commented-out #t) (node-fields node)))]
      [(node-prefix? node)
       (make-node 'prefix (node-value node)
                  (node-start node) (node-end node)
                  (cons (cons 'commented-out #t)
                        (map (lambda (field)
                               (if (and (eq? (car field) 'child) (cdr field))
                                 (cons 'child (walk (cdr field)))
                                 field))
                             (node-fields node))))]
      [else
        (make-node (node-kind node) (node-value node)
                   (node-start node) (node-end node)
                   (cons (cons 'commented-out #t) (node-fields node)))]))
  (walk node))

(define (read-datum-comment r)
  "Read \"#;\" and the one datum it comments out.

The datum is parsed in full -- its comments and locations included -- and
every node in it is marked `commented-out', which is what tells the linter
to stay out and the formatter to copy it verbatim."
  (let ((start (reader-index r)))
    (unless (scheme-family? (reader-dialect r))
      (record-error! r start 'warning
                     (format #f "the #; datum comment is not ~a syntax"
                             (dialect-name (reader-dialect r)))))
    (advance-by! r 2)
    (let loop ()
      (when (space-or-newline? (peek r))
        (advance! r)
        (loop)))
    (let ((child (read-node r)))
      (let ((end (if (and child (not (eq? child '*eof*)))
                   (child-end child)
                   (reader-index r))))
        (make-node 'prefix "#;"
                   (index->location r start)
                   (index->location r end)
                   (list (cons 'child (if (and child
                                            (not (eq? child '*eof*)))
                                        (mark-commented-out child)
                                        #f))
                         (cons 'gap "")
                         (cons 'verbatim #t)
                         (cons 'raw (substring (reader-source r)
                                               start end))))))))

(define (read-read-time-eval r)
  "\"#.\" is Common Lisp read-time evaluation: parsed, never evaluated, copied."
  (let ((start (reader-index r)))
    (unless (eq? (reader-dialect r) 'cl)
      (record-error! r start 'warning
                     "read-time evaluation `#.' is Common Lisp syntax"))
    (advance-by! r 2)
    (let loop ()
      (when (space-or-newline? (peek r))
        (advance! r)
        (loop)))
    (let ((child (read-node r)))
      (let ((end (if (and child (not (eq? child '*eof*)))
                   (child-end child)
                   (reader-index r))))
        (span-node r 'opaque start end
                   (list (cons 'verbatim #t)
                         (cons 'child (if (and child
                                            (not (eq? child '*eof*)))
                                        child #f))))))))

(define (read-unreadable r)
  "Read \"#<...>\" verbatim, tracking `<'-and-`>' nesting outside strings."
  (let ((start (reader-index r)))
    (advance-by! r 2)
    (let loop ((depth 1) (in-string? #f) (escaped? #f))
      (cond
        [(at-end? r)
         (record-error! r start 'error "unterminated #<...> object")
         (span-node r 'opaque start (reader-index r)
                    (list (cons 'verbatim #t)))]
        [else
          (let ((c (peek r)))
            (cond
              [escaped? (advance! r) (loop depth in-string? #f)]
              [(and in-string? (eqv? c #\\))
               (advance! r) (loop depth #t #t)]
              [(eqv? c #\") (advance! r) (loop depth (not in-string?) #f)]
              [in-string? (advance! r) (loop depth #t #f)]
              [(eqv? c #\<) (advance! r) (loop (+ depth 1) #f #f)]
              [(eqv? c #\>)
               (advance! r)
               (if (= depth 1)
                 (span-node r 'opaque start (reader-index r)
                            (list (cons 'verbatim #t)))
                 (loop (- depth 1) #f #f))]
              [else (advance! r) (loop depth #f #f)]))]))))

(define (read-conditional r)
  "Read \"#+feature form\" or \"#-feature form\" as one conditional node.

Neither the feature expression nor the guarded form is evaluated or
filtered: sift is a formatter, and a form that is not read on this machine
is still a form the author wrote."
  (let* ((start (reader-index r))
         (token (if (eqv? (peek-at r 1) #\+) "#+" "#-")))
    (unless (eq? (reader-dialect r) 'cl)
      (record-error! r start 'warning
                     (format #f "~a conditionals are Common Lisp syntax" token)))
    (advance-by! r 2)
    (let* ((gap-start (reader-index r)))
      (let loop ()
        (when (space-or-newline? (peek r))
          (advance! r)
          (loop)))
      (let* ((gap (substring (reader-source r) gap-start (reader-index r)))
             (feature (read-node r))
             (gap2-start (reader-index r)))
        (let loop ()
          (when (space-or-newline? (peek r))
            (advance! r)
            (loop)))
        (let* ((gap2 (substring (reader-source r) gap2-start (reader-index r)))
               (guarded (read-node r))
               (end (cond [(and guarded (not (eq? guarded '*eof*)))
                           (child-end guarded)]
                          [(and feature (not (eq? feature '*eof*)))
                           (child-end feature)]
                          [else (reader-index r)])))
          (make-node 'conditional token
                     (index->location r start)
                     (index->location r end)
                     (list (cons 'feature (if (eq? feature '*eof*)
                                            #f feature))
                           (cons 'guarded-form
                                 (if (eq? guarded '*eof*)
                                   #f guarded))
                           (cons 'gap gap)
                           (cons 'gap2 gap2))))))))

(define (read-syntax-prefix r token)
  "Read @var{token} -- a syntax-case prefix -- and the datum it applies to.

`#`' is quasisyntax, `#,' unsyntax and `#,@' unsyntax-splicing: the same three
shapes a quasiquote has, spelled with a `#' in front so that a macro can build
code without building data first.  Guile reads all three, and so does every
R6RS implementation, and so does Racket; Common Lisp and Emacs Lisp read none
of them."
  (unless (scheme-family? (reader-dialect r))
    (record-error! r (reader-index r) 'warning
                   (format #f "the ~a prefix is not ~a syntax"
                           token
                           (dialect-name (reader-dialect r)))))
  (read-prefix r token))

(define (shebang-looking? r)
  "Return @code{#t} when the `#!' at @var{r}'s index names a program to run.

A shebang holds the path of the interpreter that runs the file, so it holds a
`/': before the end of the line at the start of a file
(@code{#!/usr/bin/env guile}, @code{#! /usr/bin/guile}), or before the first
space anywhere else.  A reader directive holds neither: @code{#!r6rs},
@code{#!fold-case} and Chez's @code{#!optional} are tokens, not paths."
  (let ((start (reader-index r))
        (to-end-of-line? (zero? (reader-index r))))
    (let loop ((i (+ 2 start)))
      (cond
        [(>= i (reader-length r)) #f]
        [(char=? (string-ref (reader-source r) i) #\/) #t]
        [(or (newline-char? (string-ref (reader-source r) i))
           (and (not to-end-of-line?)
             (atom-delimiter? r (string-ref (reader-source r) i))))
         #f]
        [else (loop (+ i 1))]))))

(define (read-bang r)
  "Read a `#!' form: the shebang line at the start of a file, or a directive.

R6RS has `#!r6rs', R7RS has `#!fold-case' and `#!no-fold-case', Chez has
`#!optional' and `#!eof', Gambit has `#!void' -- all of them tokens inside a
form or at the top of one.  They are read as atoms and never rewritten; only a
`#!' that names a program path is a shebang, and that is a line of its own."
  (let ((start (reader-index r)))
    (cond
      [(not (shebang-looking? r)) (read-atom r)]
      [(zero? start) (read-shebang r)]
      [else
        (record-error! r start 'error
                       "`#!' is only a shebang at the start of a file")
        (read-atom r)])))

(define (read-char-literal r)
  "Read a `#\\a' character literal, saying so when the dialect has none.

Scheme and Common Lisp spell a character this way; Emacs Lisp spells it `?a',
so a `#\\a' in an Emacs Lisp file is reported and kept as it was written."
  (when (eq? (reader-dialect r) 'elisp)
    (record-error! r (reader-index r) 'warning
                   (format #f "character literals written `#\\a' are not ~a syntax"
                           (dialect-name (reader-dialect r)))))
  (read-scheme-char-literal r))

(define (hash-token-end r)
  "Return the index just past the token that starts at @var{r}'s index.

The token is read the way an atom is, so `#2A' ends at the bracket, `#1='
after the `=', and `#b1010' at the space."
  (let loop ((i (+ 1 (reader-index r))))
    (cond
      [(>= i (reader-length r)) i]
      [(atom-delimiter? r (string-ref (reader-source r) i)) i]
      [else (loop (+ i 1))])))

(define (glued-datum-char? c)
  "Return @code{#t} when @var{c} can begin a datum glued to its read macro.

What a read macro is glued to is a bracket, a string, another `#' form or a
quote: @code{#2A((1 2))}, @code{#c(1 2)}, @code{#p\"/tmp\"},
@code{#1=(a)} and @code{#1='(a)}.  Nothing else can follow one without a
space, so nothing else makes a token a prefix."
  (and c (memv c '(#\( #\[ #\{ #\" #\# #\' #\` #\,))))

(define (read-hash-token r)
  "Read a `#'-token as a read macro over a datum, or as a plain atom.

`#1=', `#2A', `#c', `#16r', `#s' are read macros: the datum they apply to is
glued to them, and the formatter must never put a space between the two,
because @code{#2A((1 2))} is an array and @code{#2A ((1 2))} is not.  A token
with nothing glued to it -- @code{#b1010}, @code{#*1011}, @code{#:name} -- is
an atom, and is read as one."
  (let* ((start (reader-index r))
         (end (hash-token-end r))
         (next (and (< end (reader-length r))
                 (string-ref (reader-source r) end))))
    (if (glued-datum-char? next)
      (read-prefix r (substring (reader-source r) start end))
      (read-atom r))))

(define (read-gexp-prefix r)
  "Read a Guix gexp reader prefix and the datum it applies to.

Guix extends the Scheme reader with five prefixes: `#~' for `gexp', `#$' and
`#$@' for `ungexp' and `ungexp-splicing', and `#+' and `#+@' for the two
native variants.  They are read the way a quote is -- as one prefix glued to
one datum -- so the formatter keeps them against what they apply to and the
checkers can tell a gexp from a symbol whose name happens to start with `#'.

The reader does not expand them: a file is formatted and linted as written,
and `#~(modify-phases ...)' is the shape its author chose."
  (let* ((second (peek-at r 1))
         (third (peek-at r 2)))
    (cond
      [(and (eqv? second #\$) (eqv? third #\@)) (read-prefix r "#$@")]
      [(and (eqv? second #\+) (eqv? third #\@)) (read-prefix r "#+@")]
      [(eqv? second #\~) (read-prefix r "#~")]
      [(eqv? second #\$) (read-prefix r "#$")]
      [else (read-prefix r "#+")])))

(define (read-hash r)
  (let ((start (reader-index r))
        (c (peek-at r 1)))
    (cond
      [(not c)
       (read-atom r)]
      ;; A Racket module opens with `#lang <language>', and the line is the
      ;; module's header: only at the start of the file, and only with the
      ;; space that separates the two words.
      [(and (zero? start) (looking-at? r "#lang ")) (read-language-line r)]
      [(eqv? c #\|) (read-block-comment r)]
      [(eqv? c #\;) (read-datum-comment r)]
      [(eqv? c #\') (read-prefix r "#'")]
      [(and (eqv? (peek-at r 2) #\@) (eqv? c #\,))
       (read-syntax-prefix r "#,@")]
      [(eqv? c #\,) (read-syntax-prefix r "#,")]
      [(eqv? c #\`) (read-syntax-prefix r "#`")]
      [(eqv? c #\() (read-balanced r "#(")]
      [(eqv? c #\{) (read-extended-symbol r)]
      ;; A bytevector is written `#u8(...)' by Guile and R7RS and `#vu8(...)'
      ;; by R6RS; both are the same datum, so both are read the same way.  The
      ;; spelling is kept, because the reader never respells a token.
      [(looking-at? r "#u8(") (read-bytevector r "#u8(")]
      [(looking-at? r "#vu8(") (read-bytevector r "#vu8(")]
      [(and (memv c '(#\p #\P)) (eqv? (peek-at r 2) #\"))
       (unless (eq? (reader-dialect r) 'cl)
         (record-error! r start 'warning "pathnames are Common Lisp syntax"))
       ;; the token as written -- `#p' or `#P' -- because the reader never
       ;; respells one
       (read-prefix r (substring (reader-source r) start (+ start 2)))]
      ;; `#~', `#$', `#$@', `#+' and `#+@' are Guix's gexp syntax.  In Common
      ;; Lisp the same two characters open a conditional, so which reading is
      ;; right is the dialect's question, and `#-' is a conditional in any
      ;; case: no Scheme has a use for it.
      [(and (memv c '(#\~ #\$ #\+)) (not (eq? (reader-dialect r) 'cl)))
       (read-gexp-prefix r)]
      [(memv c '(#\+ #\-))
       (read-conditional r)]
      [(eqv? c #\.) (read-read-time-eval r)]
      [(eqv? c #\<) (read-unreadable r)]
      [(eqv? c #\!) (read-bang r)]
      [(eqv? c #\\) (read-char-literal r)]
      [else (read-hash-token r)])))

;;; ===== The dispatch =====

(define (dialect-brackets dialect)
  "Return the bracket pairs @var{dialect} reads, as @code{(open . close)}.

Parentheses are a list in all four dialects.  A square bracket is a list in
Scheme, Racket and Common Lisp, and a vector in Emacs Lisp.  A curly brace
is not a bracket in any of them: Guile reads `{a b}' as the two symbols `{a'
and `b}', Emacs Lisp and Common Lisp read `{a' as one symbol, and treating a
brace as an opener would split a name in two and leave a form that never
closes.  The Schemes that do read `{...}' as parentheses keep every byte of it
here -- the braces are atoms and the text is copied -- but they do not get
bracket-aware formatting."
  (case dialect
    [(cl) '((#\( . #\)))]
    [else '((#\( . #\)) (#\[ . #\]))]))

(define (dialect-closers dialect)
  "Return the characters that close a list in @var{dialect}."
  (map cdr (dialect-brackets dialect)))

(define (read-node r)
  "Read the next node, or return @code{*eof*} at end of input."
  (cond
    [(at-end? r) '*eof*]
    [(and (= 0 (reader-index r)) (eqv? (peek r) #\xFEFF))
     ;; A byte order mark is not a datum; it is skipped so that a file saved by
     ;; an editor on another platform reads the same as one saved here.
     (advance! r)
     (read-node r)]
    [(newline-char? (peek r)) (read-newline-run r)]
    [(whitespace-char? (peek r)) (read-space-run r)]
    [(eqv? (peek r) #\;) (read-comment r)]
    [(assv (peek r) (dialect-brackets (reader-dialect r)))
     (read-balanced r (string (peek r)))]
    [(memv (peek r) (dialect-closers (reader-dialect r)))
     (read-stray-close r)]
    [(eqv? (peek r) #\") (read-string r)]
    [(eqv? (peek r) #\') (read-prefix r "'")]
    [(eqv? (peek r) #\`) (read-prefix r "`")]
    [(eqv? (peek r) #\,)
     (if (eqv? (peek-at r 1) #\@) (read-prefix r ",@") (read-prefix r ","))]
    [(eqv? (peek r) #\#) (read-hash r)]
    [(and (eqv? (peek r) #\|) (bar-symbol-dialect? r)) (read-bar-symbol r)]
    [(and (eqv? (peek r) #\?) (eq? (reader-dialect r) 'elisp))
     (read-elisp-char-literal r)]
    [else (read-atom r)]))

;;; ===== Whole-file interface =====

(define (read-port-text port)
  "Return everything @var{port} has left, as a string."
  (call-with-output-string
    (lambda (out)
      (let loop ()
        (let ((c (read-char port)))
          (unless (eof-object? c)
            (display c out)
            (loop)))))))

(define* (read-source #:key (dialect 'scheme) (file #f) (port #f) (source #f))
  "Read @var{source} (or all of @var{port}) and return the AST and the errors.

Two values come back: a list of top-level nodes, each carrying its
location, and a list of <lint-warning> records whose checker is `reader'.
A file with errors still has an AST: the reader returns everything it could
read, and never abandons the rest of the file because one form is broken.

`port' defaults to @code{#f} and the current input port is taken in the body
rather than in the argument list, because a default that calls a procedure is
evaluated on every call -- including the calls that pass @var{source} and have
no port to give -- and taking a port that will not be read is work, and can be
a side effect, for nothing."
  (let* ((text (if source
                 source
                 (read-port-text (or port (current-input-port)))))
         (r (make-reader text #:file file #:dialect dialect)))
    (let loop ((nodes '()))
      (let ((node (read-node r)))
        (if (eq? node '*eof*)
          (values (reverse nodes) (reverse (reader-errors r)))
          (loop (cons node nodes)))))))

(define* (read-source-string string #:key (dialect 'scheme) (file #f))
  "Read @var{string}; a convenience over @code{read-source} for tests."
  (read-source #:source string #:dialect dialect #:file file))

(define* (read-source/file path #:key (dialect 'scheme))
  "Read the file at @var{path}."
  (read-source #:source (call-with-input-file path get-string-all)
               #:dialect dialect
               #:file path))

(define (ast->text ast)
  "Render @var{ast} back to source text, byte for byte.

The reader and this function are inverses: every comment, every blank line
and every reader prefix in the input is in the output, in the same place."
  (call-with-output-string
    (lambda (out)
      (for-each (lambda (node) (display (node->string node) out)) ast))))

(define (string->ast-list string)
  "Read @var{string} and return only its data nodes, trivia dropped.

This is the structural view a test wants when it asks \"what did the reader
see?\": `(string->ast-list \"(a b)\")' gives one node, the list."
  (call-with-values
    (lambda () (read-source-string string))
    (lambda (ast errors)
      (filter node-data? ast))))

;;; ===== Dialect detection =====

(define (extension-of path)
  "Return the extension @var{path} ends in, a compression suffix aside.

A compressed source is the source it holds, so `foo.el.gz' has the extension
of `foo.el'."
  (let* ((path (if (string-suffix? ".gz" path) (string-drop-right path 3) path))
         (dot (string-rindex path #\.)))
    (and dot
      (let ((slash (string-rindex path #\/)))
        (and (or (not slash) (> dot slash))
          (string-downcase (substring path dot)))))))

(define (dialect-marker source)
  "Return the dialect named by a marker inside @var{source}, or @code{#f}.

The first 200 lines are searched, because a library's `define-module' or
`in-package' appears before any code it describes, and a file that names
its dialect that early has already told us what it is.  A `#lang' line comes
first among the markers because nothing else writes one: it is the whole of
what a Racket module says about itself."
  (let* ((head (string-join (take (source-lines source) (min 200
                                                             (length (source-lines source))))
                            "\n"))
         (trimmed (string-trim-both head)))
    (cond
      [(string-find trimmed "#lang ") 'racket]
      [(string-find head "lexical-binding: t") 'elisp]
      [(or (string-find trimmed "(in-package")
         (string-find trimmed "(defpackage")) 'cl]
      [(or (string-find trimmed "(define-module")
         (string-find trimmed "(use-modules")) 'scheme]
      [else #f])))

(define* (detect-dialect #:key file source)
  "Work out which dialect a file is written in.

The order is the one the manual documents: the extension first, because it
is what the user chose; then the shebang, because it says which interpreter
runs the file; then a marker inside the file; and nothing else.  A caller
that gets @code{#f} must ask for `--dialect' rather than guess."
  (or (and file (extension-dialect file))
    (and source (shebang-dialect source))
    (and source (dialect-marker source))
    #f))

(define (extension-dialect file)
  (let ((ext (extension-of file)))
    (cond
      ;; `.sld' and `.sls' are R7RS and R6RS libraries, `.sps' an R6RS
      ;; program: all three are Scheme, and a project that keeps them apart
      ;; from its `.scm' files would otherwise have to pass `--dialect' for
      ;; every one of them.
      [(and ext (member ext '(".scm" ".ss" ".sld" ".sls" ".sps"))) 'scheme]
      ;; `.rktd' is a Racket data module and `.rktl' a library the test
      ;; runner loads; `.scrbl' is Scribble, which is Racket with another
      ;; reader in front.
      [(and ext (member ext '(".rkt" ".rktd" ".rktl" ".scrbl"))) 'racket]
      [(and ext (string=? ext ".el")) 'elisp]
      [(and ext (member ext '(".lisp" ".cl" ".asd" ".lsp" ".system"))) 'cl]
      [else #f])))

;; The programs whose name in a shebang means the file is Scheme.  Guile is the
;; one sift is written for and the one the dialect is named after, but a
;; shebang naming Chez, Gauche, Chicken, Chibi, Gambit, MIT Scheme or the
;; report itself is just as good a statement that the file is Scheme.  Racket
;; is asked first, because it is the one interpreter on this list that has a
;; dialect of its own.
(define scheme-interpreters
  '("guile" "scheme" "chez" "petite" "gosh" "csi" "chibi" "gsi" "gambit"
    "r6rs" "r7rs"))

(define (shebang-dialect source)
  (and (string-prefix? "#!" source)
    (let* ((first-line (car (source-lines source)))
           (line (string-downcase first-line)))
      (cond
        [(string-find line "racket") 'racket]
        [(any (lambda (name) (string-find line name)) scheme-interpreters)
         'scheme]
        [(or (string-find line "sbcl")
           (string-find line "clisp")
           (string-find line "ecl")
           (string-find line "ccl")) 'cl]
        [(string-find line "emacs") 'elisp]
        [else #f]))))
