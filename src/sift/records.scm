;; sift --- a dialect-aware formatter and linter for Scheme, Emacs Lisp and
;; Common Lisp.
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
  #:use-module (sift strings)
  #:export (;; --- what this is ----------------------------------------------
            sift-version

            ;; --- source locations -------------------------------------------
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
            lint-checker-code
            lint-checker-for-dialect?
            lint-checker->string

            ;; --- dialects -------------------------------------------------
            dialects
            rule-codes
            rule-code
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
      [(and fa fb (not (string=? fa fb))) (string<? fa fb)]
      [(and (not fa) fb) #t]
      [(and fa (not fb)) #f]
      [(not (= (source-location-line a) (source-location-line b)))
       (< (source-location-line a) (source-location-line b))]
      [else (< (source-location-column a) (source-location-column b))])))

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
  (case [string->symbol (string-downcase string)]
    [(error) 'error]
    [(warning warn) 'warning]
    [(info) 'info]
    [else #f]))

(define (severity-rank severity)
  "Return how loud @var{severity} is: errors 3, warnings 2, info 1."
  (case severity
    [(error) 3]
    [(warning) 2]
    [(info) 1]
    [else 0]))

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
      [(and (source-location-file loc-a) (source-location-file loc-b)
         (not (string=? (source-location-file loc-a)
                        (source-location-file loc-b))))
       (string<? (source-location-file loc-a) (source-location-file loc-b))]
      [(not (eqv? (source-location-line loc-a) (source-location-line loc-b)))
       (< (source-location-line loc-a) (source-location-line loc-b))]
      [(not (eqv? (source-location-column loc-a) (source-location-column loc-b)))
       (< (source-location-column loc-a) (source-location-column loc-b))]
      [else
        (string<? (symbol->string (lint-warning-checker a))
                  (symbol->string (lint-warning-checker b)))])))

;;; ===== Lint checkers =====

(define-record-type <lint-checker>
  (%make-lint-checker name code description dialects proc severity example)
  lint-checker?
  (name lint-checker-name) ; symbol
  (code lint-checker-code) ; "SIFT012": the stable name of the rule
  (description lint-checker-description) ; string, one sentence
  (dialects lint-checker-dialects) ; '(scheme elisp cl), '(...), or 'all
  (proc lint-checker-proc) ; (lambda (ast source dialect) warnings)
  (severity lint-checker-severity) ; default severity of its findings
  (example lint-checker-example)) ; #f or an alist for `explain`

(define* (make-lint-checker name description dialects proc severity example
                            #:optional (code #f))
  "Return a checker record, with its rule code.

A checker's *name* is what a person types; its *code* is what an editor
attaches to a diagnostic so that a key press can jump to the rule's
documentation.  The code is derived from the name through @code{rule-codes}
when it is not given, so a checker cannot be written without one and a code
cannot move when the list is re-sorted."
  (%make-lint-checker name (or code (rule-code name)) description dialects
                      proc severity example))

(define sift-version "1.26.1")

(define dialects '(scheme elisp cl))

;;; ===== Rule codes =====
;;;
;;; The codes are assigned by hand and never reused: a diagnostic printed by
;;; an older sift in a bug report still means the same rule today.  New rules
;;; take the next free number, so the list grows at the end rather than
;;; being re-sorted.

(define rule-codes
  '((trailing-whitespace . "SIFT001")
    (tabs . "SIFT002")
    (line-length . "SIFT003")
    (unmatched-parens . "SIFT004")
    (empty-body . "SIFT005")
    (redundant-progn . "SIFT006")
    (reader-prefix-break . "SIFT007")
    (empty-form . "SIFT008")
    (duplicate-keyword . "SIFT009")
    (trailing-blank-lines . "SIFT010")
    (internal-define . "SIFT011")
    (if-without-else . "SIFT012")
    (when-without-body . "SIFT013")
    (unless-without-body . "SIFT014")
    (letrec-single-binding . "SIFT015")
    (quasiquote-unquote-constant . "SIFT016")
    (docstring-missing . "SIFT017")
    (docstring-first-line . "SIFT018")
    (docstring-indent . "SIFT019")
    (lexical-binding-missing . "SIFT020")
    (interactive-without-docstring . "SIFT021")
    (checkdoc-style-verb . "SIFT022")
    (provide-missing . "SIFT023")
    (declare-indent . "SIFT024")
    (package-prefix . "SIFT025")
    (double-colon . "SIFT026")
    (dangling-paren . "SIFT027")
    (defun-no-docstring . "SIFT028")
    (loop-keyword-style . "SIFT029")
    (redundant-identity . "SIFT030")
    ;; the rules added with the editor integration
    (wrap-in-let . "SIFT031")
    (keyword-arguments . "SIFT032")
    (negated-if . "SIFT033")
    (docstring-arguments . "SIFT034")
    (docstring-texinfo . "SIFT035")
    (clause-brackets . "SIFT036")
    (duplicate-binding . "SIFT037")
    (docstring-before-interactive . "SIFT038")
    (unused-binding . "SIFT039")
    (naming-convention . "SIFT040")
    (predicate-name . "SIFT041")
    (special-variable-name . "SIFT042")
    (unused-argument . "SIFT043")
    (nesting-depth . "SIFT044")
    ;; the rules about a form that says more than it does
    (eta-reduction . "SIFT045")
    (beta-reduction . "SIFT046")
    (composition . "SIFT047")
    (let-simplification . "SIFT048")
    ;; the shape of an SXML document
    (sxml-attributes . "SIFT049")
    (sxml-child . "SIFT050")
    ;; the layout rules of Riastradh's style guide
    (comment-semicolons . "SIFT051")
    (blank-line-in-body . "SIFT052")
    (single-letter-name . "SIFT053")
    ;; the rules about a form that reads as safe and is not quite
    (quote-of-constant . "SIFT054")
    (eq-on-literal . "SIFT055")
    (cond-else-not-last . "SIFT056")
    (duplicate-case-datum . "SIFT057")
    (too-many-arguments . "SIFT058")
    (defcustom-without-type . "SIFT059")
    ;; the rules about what a file claims to use
    (unused-import . "SIFT060")
    (library-not-found . "SIFT061")
    (unused-definition . "SIFT062")
    ;; the rules added after the first release of the editor integration
    (sxml-empty-element . "SIFT063")
    ;; the rules a survey of the three style guides asked for
    (apply-of-list . "SIFT064")
    (let-sibling . "SIFT065")
    (needless-let* . "SIFT066")
    (eq-nil . "SIFT067")
    (append-nothing . "SIFT068")
    (case-literal-datum . "SIFT069")
    (letrec-init-reference . "SIFT070")
    (defpackage-use . "SIFT071")
    (ecase-otherwise . "SIFT072")
    (mixed-lambda-list . "SIFT073")
    (require-cl . "SIFT074")
    (save-excursion-set-buffer . "SIFT075")
    (interactive-without-arguments . "SIFT076")
    (gexp-quoted . "SIFT077")
    (ungexp-outside-gexp . "SIFT078")
    (gexp-output-script . "SIFT079")
    (assoc-ref-outputs . "SIFT080")
    (string-append-file-append . "SIFT081")
    (phase-name-quoting . "SIFT082")
    (use-modules-in-gexp . "SIFT083")
    (package-synopsis . "SIFT084")
    (package-description-literal . "SIFT085")
    (sha256-base32 . "SIFT086")
    (inputs-label-alist . "SIFT087")
    (call-arity . "SIFT088")
    (string-literal-argument . "SIFT089")
    (number-literal-argument . "SIFT090")
    (unused-import-from . "SIFT091")
    (eq-non-symbol . "SIFT092")))

(define (rule-code name)
  "Return the code of the rule named @var{name}, or @code{#f}."
  (assq-ref rule-codes name))

(define (valid-dialect? thing)
  "Return @code{#t} when @var{thing} names a dialect sift reads."
  (and (symbol? thing) (memq thing dialects) #t))

(define (dialect->string dialect)
  (symbol->string dialect))

(define (string->dialect string)
  "Return the dialect named by @var{string}, or @code{#f}.

Both the editor-facing names and the everyday ones are accepted, so
@code{elisp} and @code{emacs-lisp} name the same dialect."
  (case [string->symbol (string-downcase string)]
    [(scheme) 'scheme]
    [(elisp emacs-lisp emacs) 'elisp]
    [(cl common-lisp lisp) 'cl]
    [else #f]))

(define (lint-checker-for-dialect? checker dialect)
  "Return @code{#t} when @var{checker} applies to @var{dialect}."
  (let ((applies (lint-checker-dialects checker)))
    (or (eq? applies 'all) (memq dialect applies) #f)))

(define (lint-checker->string checker)
  "Render @var{checker} as one line of the @code{list-checkers} table."
  (format #f "~a ~a ~a ~a"
          (pad-right (symbol->string (lint-checker-name checker)) 24)
          (pad-right (format #f "~a" (lint-checker-dialects checker)) 15)
          (pad-right (symbol->string (lint-checker-severity checker)) 10)
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
;;;   list             the list of child nodes        open, close, vector
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
;;; whose `open' is "#u8(" or "#vu8("; both close with ")".  This keeps the
;;; renderer's one rule -- "a list is its opener, its children, its closer" --
;;; true for every bracketed datum.  The `vector' field says whether the node
;;; is a vector literal, which the reader decides because it is the only pass
;;; that knows the dialect: `[' is a vector in Emacs Lisp and a clause in
;;; Scheme.
;;;
;;; A symbol written in the extended syntax Guile reads, `#{foo bar}#', is an
;;; atom like any other: its value is the text between the two markers, its
;;; type is `symbol', and the formatter copies it back unchanged.  Nothing
;;; parses what is inside it, because what is inside it is a name.
;;;
;;; Every node carries its start and end location, so that a diagnostic can
;;; point at it, a fix can replace exactly it, and a verbatim node can have
;;; its original text cut back out of the source.

(define-record-type <node>
  (%make-node kind value start end fields cache)
  node?
  (kind node-kind) ; symbol
  (value node-value) ; depends on the kind
  (start node-start) ; <source-location>
  (end node-end) ; <source-location>
  (fields node-fields) ; alist of extra properties
  (cache node-cache set-node-cache!)) ; derived from the children, computed once

;;; The `cache' slot is the one place a node is not a plain record of what the
;;; reader read.  Two questions about a list node -- which of its children
;;; carry a datum, and what the first of them is called -- are asked by every
;;; checker about every node, and both are answered by walking the children.
;;; Answering them once and keeping the answer is what turns the second ask
;;; from a walk into a vector reference; it is safe because a node never
;;; changes: the one pass that rewrites one (marking a datum comment's children
;;; commented out) builds a new node, which starts with no cache.

(define* (make-node kind value start end #:optional (fields '()))
  "Return a node of @var{kind} holding @var{value}, spanning @var{start}..@var{end}."
  (%make-node kind value start end fields #f))

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

;;; Every predicate answers #f for something that is not a node at all.  A
;;; checker asking `(node-list? (car arguments))` on a form with no arguments
;;; gets #f rather than a struct error, because a linter that raises on the
;;; file it was pointed at is useless.

(define (node-kind? thing kind)
  (and (node? thing) (eq? (node-kind thing) kind)))

(define (node-atom? node) (node-kind? node 'atom))
(define (node-list? node) (node-kind? node 'list))
(define (node-string? node)
  (and (node-atom? node) (eq? (node-atom-type node) 'string)))
(define (node-comment? node) (node-kind? node 'comment))
(define (node-block-comment? node) (node-kind? node 'block-comment))
(define (node-whitespace? node) (node-kind? node 'whitespace))
(define (node-prefix? node) (node-kind? node 'prefix))
(define (node-conditional? node) (node-kind? node 'conditional))
(define (node-shebang? node) (node-kind? node 'shebang))
(define (node-opaque? node) (node-kind? node 'opaque))
(define (node-vector? node)
  "Return @code{#t} for a vector literal.

A vector is `#(1 2)' in every dialect sift reads, and `[1 2]' in Emacs Lisp,
where a square bracket *is* the vector syntax.  In Scheme and Common Lisp a
square bracket is a list written with brackets -- a `cond' clause, most of the
time -- and is not a vector at all.

The reader settles the question when it reads the opener, because that is
where the dialect is known, and records the answer on the node; this asks the
node."
  (and (node-list? node) (node-field node 'vector #f)))

(define (node-bytevector? node)
  "Return @code{#t} for a bytevector, in either of its two spellings.

`#u8(...)' is the one Guile and R7RS write and `#vu8(...)' the one R6RS writes;
both name the same datum, so a checker asks this rather than comparing the
opener itself."
  (and (node-list? node)
    (let ((open (node-open node)))
      (or (equal? open "#u8(") (equal? open "#vu8(")))))
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
  "Return the opening token of a list node: \"(\", \"[\", \"#(\" ..."
  (and (node-list? node) (node-field node 'open "(")))

(define (node-close node)
  "Return the closing token a list node was closed with, or @code{#f}."
  (and (node-list? node) (node-field node 'close ")")))

(define (node-children node)
  "Return every child of a list node, trivia included."
  (if (node-list? node) (node-value node) '()))

;; --- derived from the children, and computed once --------------------
;;;
;;; `node-elements' and `node-head-name' are asked about the same node by every
;;; checker, and both are answered by walking its children; `node-elements' also
;;; conses a fresh list each time.  The answer is kept in the node's cache slot,
;;; so the second ask is a vector reference.  Nodes are immutable, so the cache
;;; can never be stale.

(define (node-list-cache node)
  "Return @code{#(elements head-name)} for the list @var{node}, computing it once."
  (or (node-cache node)
    (let* ((children (node-value node))
           (elements (let loop ((rest children) (kept '()))
                       (cond
                         [(null? rest) (reverse! kept)]
                         [(node-data? (car rest)) (loop (cdr rest) (cons (car rest) kept))]
                         [else (loop (cdr rest) kept)])))
           ;; A vector and a bytevector have no operator.  Answering that once,
           ;; with the children, is what keeps `node-head-name' down to one
           ;; vector reference: it was the largest single cost a lint had.
           (computed (vector elements
                             (and (not (node-vector? node))
                               (not (node-bytevector? node))
                               (head-name-of elements)))))
      (set-node-cache! node computed)
      computed)))

(define (head-name-of elements)
  "Return the operator named by the first datum of @var{elements}, or @code{#f}.

Only an unquoted, unprefixed atom names an operator: @code{('foo x)} and
@code{(#'foo x)} have no operator, which is what the indentation table keys
on.  A keyword names one too, so that `:export' and `export' compare the same
way."
  (let ((head (and (pair? elements) (car elements))))
    (and head
      (node-atom? head)
      (not (node-stray-delimiter? head))
      (let ((type (node-atom-type head)))
        (and (memq type '(symbol keyword))
          (string->symbol (node-value head)))))))

(define (node-elements node)
  "Return the children of @var{node} that carry a datum, trivia aside.

Comments and whitespace are dropped, and so is a datum comment, so the
result is the form as a program sees it.  The list is shared with every other
caller -- a node is immutable, so sharing it is free."
  (if (node-list? node)
    (vector-ref (node-list-cache node) 0)
    '()))

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
  (and (node-list? node)
    (let ((elements (vector-ref (node-list-cache node) 0)))
      (and (pair? elements) (car elements)))))

(define (node-head-name node)
  "Return the head of a list node as a symbol, or @code{#f}.

Only an unquoted, unprefixed atom names an operator: @code{('foo x)} and
@code{(#'foo x)} have no operator, which is what the indentation table keys
on.

A vector has no operator either.  @code{#(progn a)} is a literal that holds
two symbols; reading the first of them as an operator is how a linter ends up
offering to replace a vector with its contents, and a formatter ends up
indenting a literal like a call.

The answer is computed once and kept on the node: every checker asks it about
every node, and interning the same name again for each of them is the largest
single cost a lint has."
  (and (node-list? node)
    (vector-ref (node-list-cache node) 1)))

(define (node->string node)
  "Render @var{node} back to source text, verbatim.

This is the structural inverse of the reader and is what the round-trip
fixtures compare: no formatting decisions are made here at all."
  (define (emit node out)
    (cond
      [(node-list? node)
       (display (node-open node) out)
       (for-each (lambda (child) (emit child out)) (node-children node))
       (when (node-close node) (display (node-close node) out))]
      [(node-prefix? node)
       (display (node-token node) out)
       (let ((gap (node-field node 'gap ""))
             (child (node-child node)))
         (display gap out)
         (when child (emit child out)))]
      [(node-conditional? node)
       (display (node-token node) out)
       (display (node-field node 'gap "") out)
       (emit (node-feature node) out)
       (display (node-field node 'gap2 " ") out)
       (when (node-guarded-form node) (emit (node-guarded-form node) out))]
      [else (display (node-value node) out)]))
  (let ((out (open-output-string)))
    (emit node out)
    (get-output-string out)))
