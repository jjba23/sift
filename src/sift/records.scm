;; sift --- a dialect-aware formatter and linter for Scheme, Emacs Lisp and
;; Common Lisp.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; This module holds every data type the rest of sift shares: source
;; locations, lint warnings, lint checkers and -- as documented at length
;; below -- the uniform AST node used by the reader, the formatter and the
;; checkers.
;;
;; Nothing in this module does any work: it is a vocabulary, and every other
;; module imports it so that a node built by the reader is the same shape the
;; formatter renders and the checkers inspect.

(define-module (sift records)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (;; --- source locations -------------------------------------------
              <source-location>
              make-source-location
              make-source-location*
              source-location?
              source-location-file
              source-location-line
              source-location-column
              source-location-offset
              source-location-index
              source-location->string
              source-location<?
              null-source-location

              ;; --- lint warnings --------------------------------------------
              <lint-warning>
              make-lint-warning
              make-lint-warning/range
              lint-warning?
              lint-warning-checker
              lint-warning-severity
              lint-warning-message
              lint-warning-location
              lint-warning-fix
              lint-warning-range
              lint-warning->string
              lint-warning<?
              severity-rank
              severity->string
              string->severity

              ;; --- lint checkers --------------------------------------------
              <lint-checker>
              make-lint-checker
              lint-checker?
              lint-checker-name
              lint-checker-description
              lint-checker-dialects
              lint-checker-proc
              lint-checker-severity
              lint-checker-example
              lint-checker-for-dialect?
              lint-checker->string

              ;; --- dialects -------------------------------------------------
              dialects
              valid-dialect?
              dialect->string
              string->dialect

              ;; --- AST nodes ------------------------------------------------
              <node>
              make-node
              node?
              node-kind
              node-value
              node-start
              node-end
              node-fields
              node-field
              node-field/node

              node-atom? node-list? node-vector? node-bytevector?
              node-string? node-comment? node-block-comment?
              node-whitespace? node-prefix? node-datum-comment?
              node-conditional? node-shebang? node-opaque?
              node-stray-delimiter?
              node-data? ; a datum, as opposed to trivia
              node-trivia? ; whitespace or a comment
              node-verbatim?

              node-token
              node-child
              node-feature
              node-guarded-form
              node-children ; every child of a list node
              node-elements ; only the child data nodes
              node-open
              node-close
              node-inline?
              node-newlines
              node-commented-out?
              node-atom-type
              node-package-marker
              node-raw

              node-head
              node-head-name
              node->string))

;;; ===== Source locations =====
;;;
;;; A location names a position in a source file.  `line' and `column' are
;;; 1-based and the column counts characters rather than bytes, so a line of
;;; UTF-8 text reports the column a human would count.  `offset' is the
;;; 0-based byte offset, which is what a patch or an editor expects; `index'
;;; is the 0-based character index into the source string, which is what the
;;; reader and the fix procedures use because Guile's `substring' counts
;;; characters.  Both are carried so that neither consumer has to convert.

(define-record-type <source-location>
  (%make-source-location file line column offset index)
  source-location?
  (file source-location-file) ; string, or #f for stdin
  (line source-location-line) ; 1-based integer
  (column source-location-column) ; 1-based integer, in characters
  (offset source-location-offset) ; 0-based byte offset
  (index source-location-index)) ; 0-based character offset

(define* (make-source-location file line column offset #:optional (index offset))
  "Return a location, defaulting the character index to the byte OFFSET.

Both numbers are kept because a file of ASCII is the common case in which
they agree, and code that only cares about a position should not have to
pass both."
  (%make-source-location file line column offset index))

(define* (make-source-location* file line column offset index)
  "Return a location carrying both the byte OFFSET and the character INDEX."
  (%make-source-location file line column offset index))

;; The location used for a diagnostic that has no position at all.
(define null-source-location
  (make-source-location #f 1 1 0 0))

(define (source-location->string location)
  "Format @var{location} the way a compiler does: file:line:column."
  (format #f "~a:~a:~a"
    (or (source-location-file location) "<stdin>")
    (source-location-line location)
    (source-location-column location)))

(define (source-location<? a b)
  "Order two locations by file, then line, then column.

A missing file sorts first, so a diagnostic about stdin is reported before
one about a named file in the same run."
  (let ((fa (source-location-file a))
        (fb (source-location-file b)))
    (cond
      ((and fa fb (not (string=? fa fb))) (string<? fa fb))
      ((and (not fa) fb) #t)
      ((and fa (not fb)) #f)
      ((not (= (source-location-line a) (source-location-line b)))
       (< (source-location-line a) (source-location-line b)))
      (else (< (source-location-column a) (source-location-column b))))))

;;; ===== Lint warnings =====

(define-record-type <lint-warning>
  (%make-lint-warning checker severity message location fix range)
  lint-warning?
  (checker lint-warning-checker) ; symbol
  (severity lint-warning-severity) ; 'error | 'warning | 'info
  (message lint-warning-message) ; string
  (location lint-warning-location) ; <source-location>
  (fix lint-warning-fix) ; #f or (lambda (source start end) string)
  (range lint-warning-range)) ; #f or (start . end) character offsets

(define* (make-lint-warning checker severity message location
           #:optional (fix #f) (range #f))
  "Return a warning about @var{checker} at @var{location}.

@var{fix}, when given, is a procedure of three arguments -- the whole source
string, the start character offset and the end character offset -- that
returns the replacement text for that range.  The procedure is only called
when @var{range} is also known (either passed here or derived from
@var{location}), because a fix that does not know what it replaces cannot be
applied safely."
  (%make-lint-warning checker severity message location fix range))

(define* (make-lint-warning/range checker severity message location
           start end
           #:optional (fix #f))
  "Return a warning whose fix, when applied, replaces the range START..END."
  (%make-lint-warning checker severity message location fix (cons start end)))

(define (severity->string severity)
  "Return the name of @var{severity} as it is printed."
  (symbol->string severity))

(define (string->severity string)
  "Return the severity named by @var{string}, or @code{#f}."
  (case (string->symbol (string-downcase string))
    ((error) 'error)
    ((warning warn) 'warning)
    ((info) 'info)
    (else #f)))

(define (severity-rank severity)
  "Return how loud @var{severity} is: errors 3, warnings 2, info 1."
  (case severity
    ((error) 3)
    ((warning) 2)
    ((info) 1)
    (else 0)))

(define (lint-warning->string warning)
  "Render @var{warning} as one line in the Guix lint style.

The path, line and column come first so that an editor can jump to the
finding; the checker is bracketed so that a filter can find it; the message
is a complete sentence, lowercase after the bracket and without a trailing
period, so that several findings read as a list."
  (format #f "~a: ~a: [~a] ~a"
    (source-location->string (lint-warning-location warning))
    (severity->string (lint-warning-severity warning))
    (lint-warning-checker warning)
    (lint-warning-message warning)))

(define (lint-warning<? a b)
  "Order two warnings by file, line, column and checker name.

The order has to be total and independent of hash iteration order, because
the formatter and the linter promise the same output on every run."
  (let ((loc-a (lint-warning-location a))
        (loc-b (lint-warning-location b)))
    (cond
      ((and (source-location-file loc-a) (source-location-file loc-b)
         (not (string=? (source-location-file loc-a)
                (source-location-file loc-b))))
       (string<? (source-location-file loc-a) (source-location-file loc-b)))
      ((not (eqv? (source-location-line loc-a) (source-location-line loc-b)))
       (< (source-location-line loc-a) (source-location-line loc-b)))
      ((not (eqv? (source-location-column loc-a) (source-location-column loc-b)))
       (< (source-location-column loc-a) (source-location-column loc-b)))
      (else
         (string<? (symbol->string (lint-warning-checker a))
           (symbol->string (lint-warning-checker b)))))))

;;; ===== Lint checkers =====

(define-record-type <lint-checker>
  (make-lint-checker name description dialects proc severity example)
  lint-checker?
  (name lint-checker-name) ; symbol
  (description lint-checker-description) ; string, one sentence
  (dialects lint-checker-dialects) ; '(scheme elisp cl), '(...), or 'all
  (proc lint-checker-proc) ; (lambda (ast source dialect) warnings)
  (severity lint-checker-severity) ; default severity of its findings
  (example lint-checker-example)) ; #f or an alist for `explain`

(define dialects '(scheme elisp cl))

(define (valid-dialect? thing)
  "Return @code{#t} when @var{thing} names a dialect sift reads."
  (and (symbol? thing) (memq thing dialects) #t))

(define (dialect->string dialect)
  (symbol->string dialect))

(define (string->dialect string)
  "Return the dialect named by @var{string}, or @code{#f}.

Both the editor-facing names and the everyday ones are accepted, so
@code{elisp} and @code{emacs-lisp} name the same dialect."
  (case (string->symbol (string-downcase string))
    ((scheme) 'scheme)
    ((elisp emacs-lisp emacs) 'elisp)
    ((cl common-lisp lisp) 'cl)
    (else #f)))

(define (lint-checker-for-dialect? checker dialect)
  "Return @code{#t} when @var{checker} applies to @var{dialect}."
  (let ((applies (lint-checker-dialects checker)))
    (or (eq? applies 'all) (memq dialect applies) #f)))

(define (lint-checker->string checker)
  "Render @var{checker} as one line of the @code{list-checkers} table."
  (format #f "~a ~a ~a ~a"
    (string-pad-right (symbol->string (lint-checker-name checker)) 24)
    (string-pad-right (format #f "~a" (lint-checker-dialects checker)) 15)
    (string-pad-right (symbol->string (lint-checker-severity checker)) 10)
    (lint-checker-description checker)))

;;; ===== AST nodes =====
;;;
;;; A node is one record with a tag, so that every pass can walk a tree
;;; without knowing which kinds exist, and so that a new kind of trivia is a
;;; new tag rather than a new data type to thread through every signature.
;;;
;;;   kind             value                         fields
;;;   ---------------- ----------------------------- --------------------------
;;;   atom             the source text of the atom    atom-type, package-marker
;;;   list             the list of child nodes        open, close
;;;   comment          "; ..." up to, not including,  inline
;;;                    the newline
;;;   block-comment    "#| ... |#"                   --
;;;   whitespace       normalised whitespace text     newlines
;;;   prefix           the prefix token, e.g. "'"     child, gap, verbatim
;;;   conditional      "#+" or "#-"                   feature, guarded-form
;;;   shebang          the whole first line           --
;;;   opaque           text copied verbatim           --
;;;
;;; A vector is a list node whose `open' field is "#(" and a bytevector one
;;; whose `open' is "#u8("; both close with ")".  This keeps the renderer's
;;; one rule -- "a list is its opener, its children, its closer" -- true for
;;; every bracketed datum.
;;;
;;; Every node carries its start and end location, so that a diagnostic can
;;; point at it, a fix can replace exactly it, and a verbatim node can have
;;; its original text cut back out of the source.

(define-record-type <node>
  (%make-node kind value start end fields)
  node?
  (kind node-kind) ; symbol
  (value node-value) ; depends on the kind
  (start node-start) ; <source-location>
  (end node-end) ; <source-location>
  (fields node-fields)) ; alist of extra properties

(define* (make-node kind value start end #:optional (fields '()))
  "Return a node of @var{kind} holding @var{value}, spanning @var{start}..@var{end}."
  (%make-node kind value start end fields))

(define* (node-field node key #:optional (default #f))
  "Return the property @var{key} of @var{node}, or @var{default}."
  (let ((cell (assq key (node-fields node))))
    (if cell (cdr cell) default)))

(define (node-field/node default key node)
  "Return the property @var{key} of @var{node}, or @var{default}.

This is the argument order a fold wants, so a whole tree can be summarised
without writing a lambda around every accessor."
  (node-field node key default))

;; --- kind predicates ---

(define (node-atom? node) (eq? (node-kind node) 'atom))
(define (node-list? node) (eq? (node-kind node) 'list))
(define (node-string? node)
  (and (eq? (node-kind node) 'atom) (eq? (node-atom-type node) 'string)))
(define (node-comment? node) (eq? (node-kind node) 'comment))
(define (node-block-comment? node) (eq? (node-kind node) 'block-comment))
(define (node-whitespace? node) (eq? (node-kind node) 'whitespace))
(define (node-prefix? node) (eq? (node-kind node) 'prefix))
(define (node-conditional? node) (eq? (node-kind node) 'conditional))
(define (node-shebang? node) (eq? (node-kind node) 'shebang))
(define (node-opaque? node) (eq? (node-kind node) 'opaque))
(define (node-vector? node)
  "Return @code{#t} for a vector: \"#(\" in Scheme and Common Lisp, \"[\" in Emacs Lisp."
  (and (node-list? node)
    (let ((open (node-open node)))
      (or (equal? open "#(") (equal? open "[")))))
(define (node-bytevector? node)
  (and (node-list? node) (equal? (node-open node) "#u8(")))
(define (node-stray-delimiter? node)
  (and (node-atom? node) (eq? (node-atom-type node) 'stray-delimiter)))
(define (node-datum-comment? node)
  (and (node-prefix? node) (equal? (node-token node) "#;")))

(define (node-trivia? node)
  "Return @code{#t} for a node that is not a datum: whitespace or a comment."
  (memq (node-kind node) '(whitespace comment block-comment shebang)))

(define (node-data? node)
  "Return @code{#t} for a node that carries a datum, trivia aside.

A datum comment (\"#;form\") is trivia: the form it holds is preserved but
is not part of the program, and neither the formatter nor the linter may
treat it as code."
  (not (or (node-trivia? node) (node-datum-comment? node))))

(define (node-verbatim? node)
  "Return @code{#t} when the formatter must copy @var{node} without touching it."
  (or (node-field node 'verbatim #f)
    (node-opaque? node)
    (node-datum-comment? node)))

;; --- field accessors ---

(define (node-token node)
  "Return the token of a prefix or conditional node: \"'\", \"#;\", \"#+\" ..."
  (and (memq (node-kind node) '(prefix conditional))
    (node-value node)))

(define (node-child node)
  "Return the datum a prefix node applies to, or @code{#f}."
  (and (node-prefix? node) (node-field node 'child #f)))

(define (node-feature node)
  "Return the feature expression of a @code{#+}/@code{#-} conditional."
  (and (node-conditional? node) (node-field node 'feature #f)))

(define (node-guarded-form node)
  "Return the form guarded by a @code{#+}/@code{#-} conditional."
  (and (node-conditional? node) (node-field node 'guarded-form #f)))

(define (node-open node)
  "Return the opening token of a list node: \"(\", \"[\", \"{\", \"#(\" ..."
  (and (node-list? node) (node-field node 'open "(")))

(define (node-close node)
  "Return the closing token a list node was closed with, or @code{#f}."
  (and (node-list? node) (node-field node 'close ")")))

(define (node-children node)
  "Return every child of a list node, trivia included."
  (if (node-list? node) (node-value node) '()))

(define (node-elements node)
  "Return the children of @var{node} that carry a datum, trivia aside.

Comments and whitespace are dropped, and so is a datum comment, so the
result is the form as a program sees it."
  (filter node-data? (node-children node)))

(define (node-inline? node)
  "Return @code{#t} when @var{node} is a comment that follows code on its line."
  (and (node-comment? node) (node-field node 'inline #f)))

(define (node-newlines node)
  "Return the number of newline characters in a whitespace node."
  (and (node-whitespace? node) (node-field node 'newlines 0)))

(define (node-commented-out? node)
  "Return @code{#t} when @var{node} sits inside a \"#;\" datum comment."
  (node-field node 'commented-out #f))

(define (node-atom-type node)
  "Return the classification of an atom: symbol, keyword, number, string ..."
  (and (node-atom? node) (node-field node 'atom-type 'symbol)))

(define (node-package-marker node)
  "Return the package marker of a Common Lisp symbol: \"cl:\", \"cl::\", ..."
  (and (node-atom? node) (node-field node 'package-marker #f)))

(define (node-raw node)
  "Return the original source text of @var{node}, when it was recorded."
  (node-field node 'raw #f))

;; --- structural helpers ---

(define (node-head node)
  "Return the first datum of a list node, or @code{#f}."
  (let ((elements (node-elements node)))
    (if (null? elements) #f (car elements))))

(define (node-head-name node)
  "Return the head of a list node as a symbol, or @code{#f}.

Only an unquoted, unprefixed atom names an operator: @code{('foo x)} and
@code{(#'foo x)} have no operator, which is what the indentation table keys
on."
  (let ((head (node-head node)))
    (and head
      (node-atom? head)
      (not (node-stray-delimiter? head))
      (let ((type (node-atom-type head)))
        (and (memq type '(symbol keyword))
          (string->symbol (node-value head)))))))

(define (node->string node)
  "Render @var{node} back to source text, verbatim.

This is the structural inverse of the reader and is what the round-trip
fixtures compare: no formatting decisions are made here at all."
  (define (emit node out)
    (cond
      ((node-list? node)
       (display (node-open node) out)
       (for-each (lambda (child) (emit child out)) (node-children node))
       (when (node-close node) (display (node-close node) out)))
      ((node-prefix? node)
       (display (node-token node) out)
       (let ((gap (node-field node 'gap ""))
             (child (node-child node)))
         (display gap out)
         (when child (emit child out))))
      ((node-conditional? node)
       (display (node-token node) out)
       (display (node-field node 'gap "") out)
       (emit (node-feature node) out)
       (display (node-field node 'gap2 " ") out)
       (when (node-guarded-form node) (emit (node-guarded-form node) out)))
      (else (display (node-value node) out))))
  (let ((out (open-output-string)))
    (emit node out)
    (get-output-string out)))
