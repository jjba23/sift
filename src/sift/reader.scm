;; sift --- reader
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

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

(define (atom-delimiter? c)
  "Return @code{#t} when @var{c} ends an atom.

A `#' is not a delimiter: it only starts a reader directive at the beginning
of a token, and a symbol such as `foo#bar' (Scheme) is one atom."
  (and c
    (or (whitespace-char? c)
      (newline-char? c)
      (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\,)))))

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
          [else
             (let ((end (reader-index r)))
               (set-reader-index! r start)
               (let ((node (whitespace-node r start end count)))
                 (set-reader-index! r end)
                 node))])))))

(define (read-space-run r)
  (let ((start (reader-index r)))
    (while (whitespace-char? (peek r)) (advance! r))
    (let ((end (reader-index r)))
      (set-reader-index! r start)
      (let ((node (whitespace-node r start end 0)))
        (set-reader-index! r end)
        node))))

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
    (let ((end (reader-index r)))
      (set-reader-index! r start)
      (let ((node (comment-node r start end inline?)))
        (set-reader-index! r end)
        node))))

(define (read-shebang r)
  (let ((start (reader-index r)))
    (while (and (peek r) (not (newline-char? (peek r)))) (advance! r))
    (let ((end (reader-index r)))
      (set-reader-index! r start)
      (let ((node (make-node 'shebang
                    (substring (reader-source r) start end)
                    (index->location r start)
                    (index->location r end))))
        (set-reader-index! r end)
        node))))

;;; ===== Reading bracket forms =====

(define (matching-close open)
  (case open
    [(#\() #\)]
    [(#\[) #\]]
    [(#\{) #\}]
    [else #\)]))

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
         (let ((end (reader-index r)))
           (set-reader-index! r start)
           (let ((node (make-node 'list (reverse children)
                         (index->location r start)
                         (index->location r end)
                         (list (cons 'open open-text)
                           (cons 'close #f)))))
             (set-reader-index! r end)
             node))]
        [(eqv? (peek r) close-char)
         (advance! r)
         (let ((end (reader-index r)))
           (set-reader-index! r start)
           (let ((node (make-node 'list (reverse children)
                         (index->location r start)
                         (index->location r end)
                         (list (cons 'open open-text)
                           (cons 'close (string close-char))))))
             (set-reader-index! r end)
             node))]
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
         (let ((end (reader-index r)))
           (set-reader-index! r start)
           (let ((node (make-node 'atom
                         (substring (reader-source r) start end)
                         (index->location r start)
                         (index->location r end)
                         (list (cons 'atom-type 'string)))))
             (set-reader-index! r end)
             node))]
        [(eqv? (peek r) #\")
         (advance! r)
         (let ((end (reader-index r)))
           (set-reader-index! r start)
           (let ((node (make-node 'atom
                         (substring (reader-source r) start end)
                         (index->location r start)
                         (index->location r end)
                         (list (cons 'atom-type 'string)))))
             (set-reader-index! r end)
             node))]
        [(eqv? (peek r) #\\)
         ;; An escape is copied as it was written.  Nothing here decodes a
         ;; string, because the formatter must never respell an escape.
         (advance! r)
         (unless (at-end? r) (advance! r))
         (loop)]
        [else (advance! r) (loop)]))))

(define (read-scheme-char-literal r)
  "Read a Scheme character literal: #\\a, #\\space, #\\x41."
  (let ((start (reader-index r)))
    (advance-by! r 2) ; # and \
    (let ((c (peek r)))
      (cond
        [(not c) #f]
        [(and (memv c '(#\x #\X))
           (let ((n (peek-at r 1)))
             (and n (memv n '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                              #\a #\b #\c #\d #\e #\f
                              #\A #\B #\C #\D #\E #\F)))))
         (advance! r)
         (while (let ((d (peek r)))
                  (and d (memv d '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                                   #\a #\b #\c #\d #\e #\f
                                   #\A #\B #\C #\D #\E #\F))))
           (advance! r))]
        [(char-alphabetic? c)
         (while (let ((d (peek r)))
                  (and d (or (char-alphabetic? d)
                           (and (memv d '(#\- #\_))
                             (char-alphabetic? (peek-at r 1))))))
           (advance! r))]
        [else (advance! r)])
      (let ((end (reader-index r)))
        (set-reader-index! r start)
        (let ((node (make-node 'atom
                      (substring (reader-source r) start end)
                      (index->location r start)
                      (index->location r end)
                      (list (cons 'atom-type 'character)))))
          (set-reader-index! r end)
          node)))))

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
            [(eqv? c #\\) (advance! r) (unless (at-end? r) (advance! r))]
            [(eqv? c #\^) (advance! r) (unless (at-end? r) (advance! r))]
            [else (advance! r)])))
      ;; a plain character literal: ?a, ?(, ?\s
      (unless (at-end? r) (advance! r)))
    (let ((end (reader-index r)))
      (set-reader-index! r start)
      (let ((node (make-node 'atom
                    (substring (reader-source r) start end)
                    (index->location r start)
                    (index->location r end)
                    (list (cons 'atom-type 'character)))))
        (set-reader-index! r end)
        node))))

;;; ===== Atoms =====

(define (read-bar-symbol r)
  "Read a |bar-quoted symbol|, escapes included, verbatim."
  (let ((start (reader-index r)))
    (advance! r)
    (let loop ()
      (cond
        [(at-end? r)
         (record-error! r start 'error "unterminated |symbol|")
         (let ((end (reader-index r)))
           (set-reader-index! r start)
           (let ((node (make-node 'atom
                         (substring (reader-source r) start end)
                         (index->location r start)
                         (index->location r end)
                         (list (cons 'atom-type 'symbol)))))
             (set-reader-index! r end)
             node))]
        [(eqv? (peek r) #\\) (advance! r) (unless (at-end? r) (advance! r)) (loop)]
        [(eqv? (peek r) #\|) (advance! r)
         (let ((end (reader-index r)))
           (set-reader-index! r start)
           (let ((node (make-node 'atom
                         (substring (reader-source r) start end)
                         (index->location r start)
                         (index->location r end)
                         (list (cons 'atom-type 'symbol)))))
             (set-reader-index! r end)
             node))]
        [else (advance! r) (loop)]))))

(define (read-atom r)
  (let ((start (reader-index r)))
    (while (and (peek r) (not (atom-delimiter? (peek r)))) (advance! r))
    (when (= start (reader-index r)) ; a lone delimiter we do not know
      (advance! r))
    (let ((end (reader-index r)))
      (set-reader-index! r start)
      (let ((node (atom-node r (substring (reader-source r) start end)
                    start end)))
        (set-reader-index! r end)
        node))))

(define (atom-node r text start end)
  "Classify @var{text} and return the node that stands for it."
  (let* ((number (and (not (string-prefix? "|" text))
                   (string->number text)))
         (type (cond
                 [number 'number]
                 [(member text '("#t" "#f" "#true" "#false")) 'boolean]
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
      (if (and (peek r) (or (whitespace-char? (peek r))
                          (newline-char? (peek r))))
        (begin (advance! r) (loop))))
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
           (let ((end child-start))
             (set-reader-index! r start)
             (let ((node (make-node 'prefix token
                           (index->location r start)
                           (index->location r end)
                           (list (cons 'child #f)
                             (cons 'gap "")))))
               (set-reader-index! r end)
               node))]
          [else
             (let ((end (child-end child)))
               (set-reader-index! r start)
               (let ((node (make-node 'prefix token
                             (index->location r start)
                             (index->location r end)
                             (list (cons 'child child)
                               (cons 'gap gap)))))
                 (set-reader-index! r end)
                 node))])))))

(define (child-end node)
  "Return the character index just past @var{node}."
  (source-location-index (node-end node)))

(define (read-block-comment r)
  (let ((start (reader-index r)))
    (when (eq? (reader-dialect r) 'elisp)
      (record-error! r start 'warning
        "block comments are not Emacs Lisp syntax"))
    (advance-by! r 2)
    (let loop ((depth 1))
      (cond
        [(at-end? r)
         (record-error! r start 'error "unterminated block comment")
         (let ((end (reader-index r)))
           (set-reader-index! r start)
           (let ((node (make-node 'block-comment
                         (substring (reader-source r) start end)
                         (index->location r start)
                         (index->location r end))))
             (set-reader-index! r end)
             node))]
        [(and (eqv? (peek r) #\|) (eqv? (peek-at r 1) #\#))
         (advance-by! r 2)
         (if (= depth 1)
           (let ((end (reader-index r)))
             (set-reader-index! r start)
             (let ((node (make-node 'block-comment
                           (substring (reader-source r) start end)
                           (index->location r start)
                           (index->location r end))))
               (set-reader-index! r end)
               node))
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
    (when (eq? (reader-dialect r) 'elisp)
      (record-error! r start 'warning
        "the #; datum comment is not Emacs Lisp syntax"))
    (advance-by! r 2)
    (let loop ()
      (if (and (peek r) (or (whitespace-char? (peek r))
                          (newline-char? (peek r))))
        (begin (advance! r) (loop))))
    (let ((child (read-node r)))
      (let ((end (if (and child (not (eq? child '*eof*)))
                   (child-end child)
                   (reader-index r))))
        (set-reader-index! r start)
        (let ((node (make-node 'prefix "#;"
                      (index->location r start)
                      (index->location r end)
                      (list (cons 'child (if (and child
                                               (not (eq? child '*eof*)))
                                           (mark-commented-out child)
                                           #f))
                        (cons 'gap "")
                        (cons 'verbatim #t)
                        (cons 'raw (substring (reader-source r)
                                     start end))))))
          (set-reader-index! r end)
          node)))))

(define (read-read-time-eval r)
  "\"#.\" is Common Lisp read-time evaluation: parsed, never evaluated, copied."
  (let ((start (reader-index r)))
    (advance-by! r 2)
    (let loop ()
      (if (and (peek r) (or (whitespace-char? (peek r))
                          (newline-char? (peek r))))
        (begin (advance! r) (loop))))
    (let ((child (read-node r)))
      (let ((end (if (and child (not (eq? child '*eof*)))
                   (child-end child)
                   (reader-index r))))
        (set-reader-index! r start)
        (let ((node (make-node 'opaque
                      (substring (reader-source r) start end)
                      (index->location r start)
                      (index->location r end)
                      (list (cons 'verbatim #t)
                        (cons 'child (if (and child
                                           (not (eq? child '*eof*)))
                                       child #f))))))
          (set-reader-index! r end)
          node)))))

(define (read-unreadable r)
  "Read \"#<...>\" verbatim, tracking `<'-and-`>' nesting outside strings."
  (let ((start (reader-index r)))
    (advance-by! r 2)
    (let loop ((depth 1) (in-string? #f) (escaped? #f))
      (cond
        [(at-end? r)
         (record-error! r start 'error "unterminated #<...> object")
         (let ((end (reader-index r)))
           (set-reader-index! r start)
           (let ((node (make-node 'opaque
                         (substring (reader-source r) start end)
                         (index->location r start)
                         (index->location r end)
                         (list (cons 'verbatim #t)))))
             (set-reader-index! r end)
             node))]
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
                  (let ((end (reader-index r)))
                    (set-reader-index! r start)
                    (let ((node (make-node 'opaque
                                  (substring (reader-source r) start end)
                                  (index->location r start)
                                  (index->location r end)
                                  (list (cons 'verbatim #t)))))
                      (set-reader-index! r end)
                      node))
                  (loop (- depth 1) #f #f))]
               [else (advance! r) (loop depth #f #f)]))]))))

(define (read-conditional r)
  "Read \"#+feature form\" or \"#-feature form\" as one conditional node.

Neither the feature expression nor the guarded form is evaluated or
filtered: sift is a formatter, and a form that is not read on this machine
is still a form the author wrote."
  (let* ((start (reader-index r))
         (token (if (eqv? (peek-at r 1) #\+) "#+" "#-")))
    (when (not (eq? (reader-dialect r) 'cl))
      (record-error! r start 'warning
        (format #f "~a conditionals are Common Lisp syntax" token)))
    (advance-by! r 2)
    (let* ((gap-start (reader-index r)))
      (let loop ()
        (if (and (peek r) (or (whitespace-char? (peek r))
                            (newline-char? (peek r))))
          (begin (advance! r) (loop))))
      (let* ((gap (substring (reader-source r) gap-start (reader-index r)))
             (feature (read-node r))
             (gap2-start (reader-index r)))
        (let loop ()
          (if (and (peek r) (or (whitespace-char? (peek r))
                              (newline-char? (peek r))))
            (begin (advance! r) (loop))))
        (let* ((gap2 (substring (reader-source r) gap2-start (reader-index r)))
               (guarded (read-node r))
               (end (cond [(and guarded (not (eq? guarded '*eof*)))
                           (child-end guarded)]
                          [(and feature (not (eq? feature '*eof*)))
                           (child-end feature)]
                          [else (reader-index r)])))
          (set-reader-index! r start)
          (let ((node (make-node 'conditional token
                        (index->location r start)
                        (index->location r end)
                        (list (cons 'feature (if (eq? feature '*eof*)
                                               #f feature))
                          (cons 'guarded-form
                            (if (eq? guarded '*eof*)
                              #f guarded))
                          (cons 'gap gap)
                          (cons 'gap2 gap2)))))
            (set-reader-index! r end)
            node))))))

(define (read-hash r)
  (let ((start (reader-index r))
        (c (peek-at r 1)))
    (cond
      [(not c)
       (read-atom r)]
      [(eqv? c #\|) (read-block-comment r)]
      [(eqv? c #\;) (read-datum-comment r)]
      [(eqv? c #\') (read-prefix r "#'")]
      [(eqv? c #\() (read-balanced r "#(")]
      [(and (memv c '(#\u #\U)) (eqv? (peek-at r 2) #\8) (eqv? (peek-at r 3) #\())
       (when (eq? (reader-dialect r) 'elisp)
         (record-error! r start 'warning
           "bytevectors are not Emacs Lisp syntax"))
       (read-balanced r "#u8(")]
      [(and (memv c '(#\s #\S)) (eqv? (peek-at r 2) #\())
       (read-prefix r "#s")]
      [(and (memv c '(#\p #\P)) (eqv? (peek-at r 2) #\"))
       (when (not (eq? (reader-dialect r) 'cl))
         (record-error! r start 'warning "pathnames are Common Lisp syntax"))
       (read-prefix r "#p")]
      [(memv c '(#\+ #\-))
       (read-conditional r)]
      [(eqv? c #\.) (read-read-time-eval r)]
      [(eqv? c #\<) (read-unreadable r)]
      [(eqv? c #\!)
       (if (= start 0)
         (read-shebang r)
         (begin
           (record-error! r start 'error "`#!' is only a shebang at the start of a file")
           (read-atom r)))]
      [(eqv? c #\\) (read-scheme-char-literal r)]
      [else (read-atom r)])))

;;; ===== The dispatch =====

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
    [(memv (peek r) '(#\( #\[ #\{))
     (read-balanced r (string (peek r)))]
    [(memv (peek r) '(#\) #\] #\})) (read-stray-close r)]
    [(eqv? (peek r) #\") (read-string r)]
    [(eqv? (peek r) #\') (read-prefix r "'")]
    [(eqv? (peek r) #\`) (read-prefix r "`")]
    [(eqv? (peek r) #\,)
     (if (eqv? (peek-at r 1) #\@) (read-prefix r ",@") (read-prefix r ","))]
    [(eqv? (peek r) #\#) (read-hash r)]
    [(eqv? (peek r) #\|) (read-bar-symbol r)]
    [(and (eqv? (peek r) #\?) (eq? (reader-dialect r) 'elisp))
     (read-elisp-char-literal r)]
    [else (read-atom r)]))

;;; ===== Whole-file interface =====

(define* (read-source #:key (dialect 'scheme) (file #f)
           (port (current-input-port)) (source #f)
           #:allow-other-keys)
  "Read @var{source} (or all of @var{port}) and return the AST and the errors.

Two values come back: a list of top-level nodes, each carrying its
location, and a list of <lint-warning> records whose checker is `reader'.
A file with errors still has an AST: the reader returns everything it could
read, and never abandons the rest of the file because one form is broken."
  (let* ((text (if source
                 source
                 (call-with-output-string
                   (lambda (out)
                     (let loop ()
                       (let ((c (read-char port)))
                         (unless (eof-object? c)
                           (display c out)
                           (loop))))))))
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
  (let ((dot (string-rindex path #\.)))
    (and dot
      (let ((slash (string-rindex path #\/)))
        (and (or (not slash) (> dot slash))
          (string-downcase (substring path dot)))))))

(define (dialect-marker source)
  "Return the dialect named by a marker inside @var{source}, or @code{#f}.

The first 200 lines are searched, because a library's `define-module' or
`in-package' appears before any code it describes, and a file that names
its dialect that early has already told us what it is."
  (let* ((head (string-join (take (source-lines source) (min 200
                                                          (length (source-lines source))))
                 "\n"))
         (trimmed (string-trim-both head)))
    (cond
      [(string-contains head "lexical-binding: t") 'elisp]
      [(or (string-contains trimmed "(in-package")
         (string-contains trimmed "(defpackage")) 'cl]
      [(or (string-contains trimmed "(define-module")
         (string-contains trimmed "(use-modules")) 'scheme]
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
      [(and ext (member ext '(".scm" ".ss" ".sld" ".sls"))) 'scheme]
      [(and ext (string=? ext ".el")) 'elisp]
      [(and ext (member ext '(".lisp" ".cl" ".asd" ".lsp" ".system"))) 'cl]
      [else #f])))

(define (shebang-dialect source)
  (and (string-prefix? "#!" source)
    (let* ((first-line (car (source-lines source)))
           (line (string-downcase first-line)))
      (cond
        [(or (string-contains line "guile")) 'scheme]
        [(or (string-contains line "sbcl")
           (string-contains line "clisp")
           (string-contains line "ecl")
           (string-contains line "ccl")) 'cl]
        [(or (string-contains line "emacs")) 'elisp]
        [else #f]))))
