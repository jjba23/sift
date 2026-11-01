;; sift --- YAML
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A small, data-only YAML reader and writer, sized to what a `sift.yaml'
;; configuration file needs: block mappings, block sequences, quoted and
;; plain scalars, inline `[a, b]' lists, comments, and nesting by
;; indentation.
;;
;; It is deliberately not a general YAML implementation.  Anything it does
;; not understand -- anchors, tags, block scalars, flow mappings spanning
;; lines -- is reported as an error with a line and a column rather than
;; guessed at, because a configuration file that quietly means something
;; other than what it says is worse than one that refuses to load.
;;
;; The reader keeps positions, so a wrong key or a wrong value can be
;; reported as `sift.yaml:12:5: message' instead of just a line number.

(define-module (sift yaml)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:export (yaml->scm
            yaml->nodes
            scm->yaml
            yaml-error?
            yaml-error-path
            yaml-error-line
            yaml-error-column
            yaml-error-message))

;;; ===== Errors =====

(define-record-type <yaml-error>
  (make-yaml-error path line column message)
  yaml-error?
  (path yaml-error-path)
  (line yaml-error-line)
  (column yaml-error-column)
  (message yaml-error-message))

(define (yaml-error path line column message)
  "Raise a configuration error, which every caller reports as file:line:col.

`throw' is used rather than `raise' because the object is plain data, not a
<condition>: sift wants to report it, not to hand it to Guile's condition
machinery."
  (throw 'sift-config-error (make-yaml-error path line column message)))

;;; ===== A line-oriented scanner =====

;; A line is (indent content line-number column-of-content).
(define (yaml-lines source path)
  (let loop ((raw (string-split source #\newline)) (number 1) (out '()))
    (if (null? raw)
      (reverse out)
      (let* ((line (car raw))
             (indent (or (string-index line (lambda (c)
                                              (not (memv c '(#\space #\tab)))))
                       (string-length line)))
             (content (substring line indent)))
        (loop (cdr raw)
              (+ number 1)
              (if (or (string-null? (string-trim-both content))
                    (char=? (string-ref (string-trim-both content) 0) #\;)
                    (char=? (string-ref (string-trim-both content) 0) #\#))
                out
                (cons (list indent content number (+ indent 1)) out)))))))

;; Note: `#` starts a comment in YAML, `;` does not; both are ignored here
;; only at the start of a line, and inline comments are stripped when a
;; scalar is read.

(define* (find-unquoted text pred #:optional (start 0))
  "Return the index of the first character of @var{text} that @var{pred} accepts.

Only characters outside quotes are offered to @var{pred}, and a backslash
inside a double quoted scalar skips the character after it, so a `#' inside
`\"a # b\"' is part of the string rather than the start of a comment.
@code{#f} when nothing matches.

Three procedures below ask this one question about a line -- where does the
comment start, is there a `:' that separates a key from its value, which one
-- and each used to carry its own copy of this walk.  A copy is a place for
the quoting rules to disagree with themselves."
  (let loop ((i start) (quote-char #f))
    (cond
      [(>= i (string-length text)) #f]
      [else
        (let ((c (string-ref text i)))
          (cond
            [(eq? quote-char #\")
             (cond
               [(char=? c #\\) (loop (+ i 2) quote-char)]
               [(char=? c #\") (loop (+ i 1) #f)]
               [else (loop (+ i 1) quote-char)])]
            [(eq? quote-char #\')
             (if (char=? c #\') (loop (+ i 1) #f) (loop (+ i 1) quote-char))]
            [(memv c '(#\" #\')) (loop (+ i 1) c)]
            [(pred text i c) i]
            [else (loop (+ i 1) quote-char)]))])))

(define (strip-comment text)
  "Return @var{text} without a trailing `# comment', quotes respected."
  (define (comment-start? text i c)
    (and (char=? c #\#)
      (or (= i 0) (char=? (string-ref text (- i 1)) #\space))))
  (let ((at (find-unquoted text comment-start?)))
    (if at (string-trim-right (substring text 0 at)) text)))

(define (quoted? text)
  (and (> (string-length text) 1)
    (memv (string-ref text 0) '(#\" #\'))
    (char=? (string-ref text (- (string-length text) 1))
            (string-ref text 0))))

(define (unquote-scalar text)
  (if (quoted? text)
    (let ((body (substring text 1 (- (string-length text) 1))))
      (if (char=? (string-ref text 0) #\')
        (string-replace-substring body "''" "'")
        (string-replace-substring
         (string-replace-substring
          (string-replace-substring body "\\\"" "\"")
          "\\\\" "\\")
         "\\n" "\n")))
    text))

(define (scalar->value text)
  "Return the Scheme value of a YAML scalar @var{text}."
  (let* ((clean (string-trim-both text))
         (raw (unquote-scalar clean)))
    (cond
      [(quoted? clean) raw]
      [(member clean '("true" "yes" "on" "True" "TRUE")) #t]
      [(member clean '("false" "no" "off" "False" "FALSE")) #f]
      [(member clean '("null" "~" "Null" "NULL")) 'null]
      [(string->number clean) (string->number clean)]
      [(and (string-prefix? "[" clean) (string-suffix? "]" clean))
       (let ((body (string-trim-both
                    (substring clean 1 (- (string-length clean) 1)))))
         ;; `[]' is the empty sequence, not a sequence holding one empty
         ;; string: a configuration that says `exclude-checkers: []' means
         ;; "none", and reading it as `("")' made every key that takes a list
         ;; of names unusable in the flow style.
         (if (string-null? body)
           '()
           (map scalar->value (string-split body #\,))))]
      [else raw])))

;;; ===== The parser =====
;;
;; Nodes are vectors so that a caller can tell a mapping from a sequence
;; even when both are lists:
;;   #(scalar value line column)
;;   #(sequence (node ...) line column)
;;   #(mapping ((key . node) ...) line column)

(define (scalar-node value line column) (vector 'scalar value line column))
(define (sequence-node items line column) (vector 'sequence items line column))
(define (mapping-node entries line column) (vector 'mapping entries line column))

(define (node-type node) (vector-ref node 0))
(define (parse-block lines path indent)
  "Parse the block at @var{indent}, returning a node and the remaining lines."
  (cond
    [(null? lines) (values (scalar-node 'null 1 1) '())]
    [(sequence-line? (car lines)) (parse-sequence lines path indent)]
    [else (parse-mapping lines path indent)]))

(define (sequence-line? line)
  (let ((content (second line)))
    (or (string=? content "-")
      (string-prefix? "- " content))))

(define (second line) (list-ref line 1))
(define (first-of line) (list-ref line 0))
(define (third-of line) (list-ref line 2))
(define (fourth-of line) (list-ref line 3))

(define (parse-sequence lines path indent)
  (let ((line (car lines)))
    (let loop ((rest lines) (items '()))
      (if (and (pair? rest)
            (= (first-of (car rest)) indent)
            (sequence-line? (car rest)))
        (let* ((current (car rest))
               (content (second current))
               (rest-text (string-trim (substring content 1)))
               (column (+ (first-of current) 2))
               (next (cdr rest)))
          (cond
            [(string-null? rest-text)
             (if (and (pair? next) (> (first-of (car next)) indent))
               (call-with-values
                 (lambda () (parse-block next path (first-of (car next))))
                 (lambda (node remaining)
                   (loop remaining (cons node items))))
               (loop next
                     (cons (scalar-node 'null (third-of current) column)
                           items)))]
            [(mapping-line? rest-text)
             ;; "- key: value" starts a mapping at the item's column.
             (call-with-values
               (lambda ()
                 (parse-mapping (cons (list column rest-text
                                            (third-of current) column)
                                      next)
                                path column))
               (lambda (node remaining)
                 (loop remaining (cons node items))))]
            [else
              (loop next
                    (cons (scalar-node (scalar->value rest-text)
                                       (third-of current) column)
                          items))]))
        (values (sequence-node (reverse items)
                               (third-of line) (fourth-of line))
                rest)))))

(define (mapping-line? text)
  "Return @code{#t} when @var{text} is a `key: value' line.

The colon that makes it one has to be outside quotes and followed by a space
or the end of the line, which is what tells `key: value' from `12:30'."
  (define (key-colon? text i c)
    (and (char=? c #\:)
      (or (= (+ i 1) (string-length text))
        (char=? (string-ref text (+ i 1)) #\space))))
  (and (find-unquoted text key-colon?) #t))

(define (split-mapping-line text)
  "Return @code{(key . value)} for a `key: value' line."
  (define (any-colon? text i c) (char=? c #\:))
  (let ((at (find-unquoted text any-colon?)))
    (if at
      (cons (string-trim-both (substring text 0 at))
            (string-trim-both (substring text (+ at 1))))
      (yaml-error #f 0 0 "a mapping entry needs a `:'"))))

(define (parse-mapping lines path indent)
  (let ((line (car lines)))
    (let loop ((rest lines) (entries '()))
      (if (and (pair? rest)
            (= (first-of (car rest)) indent)
            (not (sequence-line? (car rest))))
        (let* ((current (car rest))
               (content (second current))
               (pairs (split-mapping-line content))
               (key (unquote-scalar (car pairs)))
               (value-text (strip-comment (cdr pairs)))
               (column (+ (first-of current) 1))
               (next (cdr rest)))
          (cond
            [(string-null? value-text)
             (cond
               [(and (pair? next) (> (first-of (car next)) indent))
                (call-with-values
                  (lambda () (parse-block next path (first-of (car next))))
                  (lambda (node remaining)
                    (loop remaining (cons (cons key node) entries))))]
               [(and (pair? next)
                  (= (first-of (car next)) indent)
                  (sequence-line? (car next)))
                ;; A sequence may be written at the key's own indentation.
                (call-with-values
                  (lambda () (parse-sequence next path indent))
                  (lambda (node remaining)
                    (loop remaining (cons (cons key node) entries))))]
               [else
                 (loop next
                       (cons (cons key (scalar-node 'null (third-of current)
                                                    column))
                             entries))])]
            [else
              (loop next
                    (cons (cons key (scalar-node (scalar->value value-text)
                                                 (third-of current) column))
                          entries))]))
        (values (mapping-node (reverse entries)
                              (third-of line) (fourth-of line))
                rest)))))

(define* (yaml->nodes source #:key (path "<yaml>"))
  "Parse @var{source} and return its node tree, positions included."
  (let ((lines (yaml-lines source path)))
    (if (null? lines)
      (mapping-node '() 1 1)
      (call-with-values
        (lambda () (parse-block lines path (first-of (car lines))))
        (lambda (node remaining)
          node)))))

(define (node->scm node)
  (case [node-type node]
    [(scalar) (vector-ref node 1)]
    [(sequence) (map node->scm (vector-ref node 1))]
    [(mapping)
     (map (lambda (entry) (cons (car entry) (node->scm (cdr entry))))
          (vector-ref node 1))]
    [else (vector-ref node 1)]))

(define* (yaml->scm source #:key (path "<yaml>"))
  "Parse @var{source} and return plain data: alists, lists and scalars.

Keys come back as strings, because a YAML key is text; the caller decides
what a key means."
  (node->scm (yaml->nodes source #:path path)))

;;; ===== Writing =====

(define (needs-quotes? text)
  (or (string-null? text)
    (string-index text (char-set #\: #\# #\" #\' #\[ #\] #\{ #\} #\, #\&
                                 #\* #\? #\| #\- #\< #\> #\= #\! #\% #\@))
    (string-prefix? " " text)
    (string-suffix? " " text)
    (string->number text)
    (member text '("true" "false" "null" "yes" "no" "on" "off"))))

(define (write-scalar value port)
  (cond
    [(eq? value #t) (display "true" port)]
    [(eq? value #f) (display "false" port)]
    [(eq? value 'null) (display "null" port)]
    [(number? value) (display value port)]
    [(symbol? value) (write-scalar (symbol->string value) port)]
    [(string? value)
     (if (needs-quotes? value)
       (format port "~s" value)
       (display value port))]
    [else (format port "~s" value)]))

(define* (scm->yaml datum #:key (port (current-output-port)) (indent 0))
  "Write @var{datum} as YAML, one block at a time.

Only the shapes a configuration holds are written: an alist is a mapping, a
list is a sequence, everything else is a scalar."
  (let ((pad (make-string indent #\space)))
    (cond
      [(and (list? datum) (every pair? datum))
       (for-each
        (lambda (entry)
          (let ((key (car entry)) (value (cdr entry)))
            (cond
              [(and (list? value) (pair? value) (every pair? value))
               (format port "~a~a:~%" pad key)
               (scm->yaml value #:port port #:indent (+ indent 2))]
              [(and (list? value) (pair? value) (not (pair? (car value))))
               (format port "~a~a:~%" pad key)
               (for-each (lambda (item)
                           (format port "~a- " pad)
                           (write-scalar item port)
                           (newline port))
                         value)]
              [(and (list? value) (null? value))
               (format port "~a~a: []~%" pad key)]
              [else
                (format port "~a~a: " pad key)
                (write-scalar value port)
                (newline port)])))
        datum)
       *unspecified*]
      [(and (list? datum) (null? datum))
       (format port "~a[]~%" pad)]
      [else (format port "~a" pad) (write-scalar datum port) (newline port)])))
