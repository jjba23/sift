;; sift --- formatter
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; sift is a *fuzzy* formatter: it adjusts indentation, spacing and the
;; placement of delimiters, and it keeps the author's line breaks wherever
;; they carry structure.  It never reflows a form, never re-spells a string
;; escape, never moves a comment to another line, and never drops one.
;;
;; The pipeline is:
;;
;;   1. normalise line endings to LF (the reader reads CRLF as one newline);
;;   2. normalise intra-form spacing -- one space between adjacent forms on a
;;      line, none after an opener or before a closer;
;;   3. place closing delimiters (community: trailing; dangling: own line);
;;   4. compute indentation from the dialect's special-form table, with the
;;      generic rule as the fallback;
;;   5. align inline comments, when asked;
;;   6. preserve blank lines, collapsing runs that are longer than the style
;;      allows;
;;   7. clamp reader prefixes to their datum;
;;   8. leave line length to the linter, which warns rather than reflowing.
;;
;; Formatting is deterministic and idempotent: given the same input and the
;; same options it produces the same output, and formatting that output again
;; changes nothing.  Indentation is computed from structure and from the
;; column of the opening delimiter, never from the whitespace that was there,
;; which is exactly what makes the second pass a no-op.

(define-module (sift format)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:export (format-source
            format-string
            format-ast
            format/file
            format-check
            styles
            valid-style?
            string->style
            brackets-styles
            valid-brackets?
            string->brackets
            default-brackets
            blank-lines-styles
            valid-blank-lines?
            string->blank-lines
            default-blank-lines
            indent-rule
            indent-rules
            default-line-length
            line-length-for
            default-indent
            significant-nodes))

;;; ===== Options and styles =====

(define styles '(community dangling))

(define (valid-style? style)
  (and (symbol? style) (memq style styles) #t))

(define (string->style string)
  (let ((s (string->symbol (string-downcase string))))
    (and (memq s styles) s)))

(define* (default-line-length dialect)
  "Return the soft line limit sift uses for @var{dialect} when none is given.

Emacs Lisp is held to 80 columns -- the width the Emacs manual assumes for a
docstring's first line and for a source file read side by side -- while
Scheme and Common Lisp get 96."
  (if (eq? dialect 'elisp) 80 96))

(define* (line-length-for dialect #:optional (override #f))
  (or override (default-line-length dialect)))

(define* (default-brackets dialect)
  "Return the bracket style @var{dialect} wants when none is given.

Guile's own sources write a `cond' clause as `[(= x 1) ...]', and so does the
Guix code base; a Scheme file therefore gets square brackets for its clause
lists unless the caller says otherwise.  Emacs Lisp and Common Lisp have no
such convention, so they keep whatever the author wrote."
  (if (eq? dialect 'scheme) 'square 'preserve))

(define brackets-styles '(square parens preserve))

(define (valid-brackets? style)
  (and (symbol? style) (memq style brackets-styles) #t))

(define (string->brackets string)
  (let ((style (string->symbol (string-downcase string))))
    (and (memq style brackets-styles) style)))

(define blank-lines-styles '(preserve one))

(define (valid-blank-lines? style)
  (and (symbol? style) (memq style blank-lines-styles) #t))

(define (string->blank-lines string)
  (let ((style (string->symbol (string-downcase string))))
    (and (memq style blank-lines-styles) style)))

(define default-blank-lines 'preserve)

(define* (default-indent dialect)
  "Return the indentation width sift uses by default: two, for every dialect."
  2)

;;; ===== Indentation rules =====
;;;
;;; A rule is an integer -- the offset, in columns, from the opening delimiter
;;; where a child on a fresh line starts.  `data' aligns children under the
;;; first one, which is the rule a list whose head is not a symbol gets by
;;; definition: a binding list, a `cond' clause, a vector.
;;;
;;; Every form that can appear with its operator alone on the first line is
;;; listed, because the generic rule would otherwise indent it from the
;;; operator's column and drift away from the community style.
;;;
;;; `use-package' is the clearest case of that in Emacs Lisp, and the reason it
;;; has an entry of its own: its arguments are not arguments but clauses --
;;; `:ensure', `:init', `:config', `:bind' and the rest -- each with its own
;;; values under it.  Emacs indents the form like a `defun', because
;;; `use-package' says so itself (`(declare (indent defun))'), which puts every
;;; keyword, and every body form under `:init' or `:config', two columns in
;;; from the opener:
;;;
;;;   (use-package evil
;;;     :ensure t
;;;     :config
;;;     (evil-mode 1))
;;;
;;; That is how the community writes it, and it is the form a reader's eye is
;;; looking for.  Without the entry the generic rule would line the keywords up
;;; under the package name -- thirteen columns in for `(use-package evil' --
;;; which is what a call does with its arguments and what a form made of
;;; clauses is not.

(define scheme-indent-rules
  '((lambda . 2) (lambda* . 2) (case-lambda . 2)
    (define . 2) (define* . 2) (define-public . 2) (define-syntax . 2)
    (define-syntax-rule . 2) (define-record-type . 2) (define-values . 2)
    (define-module . 2) (define-class . 2) (define-generic . 2)
    (define-method . 2) (define-inlinable . 2) (define-constant . 2)
    ;; the library form of R6RS, and the declarations R7RS puts inside one
    (library . 2) (define-library . 2) (import . 2) (export . 2)
    (include . 2) (include-ci . 2) (include-library-declarations . 2)
    ;; syntax-case and the expander forms R6RS and Guile share
    (eval-when . 2) (with-syntax . 2) (syntax-parameterize . 2)
    (define-syntax-parameter . 2) (identifier-syntax . 2)
    (quasisyntax . 2) (unsyntax . 2) (unsyntax-splicing . 2)
    (let . 2) (let* . 2) (letrec . 2) (letrec* . 2) (let-values . 2)
    (let*-values . 2) (let-syntax . 2) (letrec-syntax . 2)
    (let-optionals . 2) (let-keywords . 2)
    (cond . clauses) (case . 2) (cond-expand . 2)
    (when . 2) (unless . 2) (if . 2) (and . 2) (or . 2)
    (begin . 2) (do . 2) (delay . 2) (delay-force . 2) (force . 2)
    (parameterize . 2) (with-exception-handler . 2) (guard . 2)
    (dynamic-wind . 2) (call-with-values . 2)
    (call-with-current-continuation . 2) (call/cc . 2)
    (with-input-from-file . 2) (with-output-to-file . 2)
    (call-with-output-string . 2) (call-with-input-string . 2)
    (quasiquote . 2) (unquote . 2) (unquote-splicing . 2) (quote . 2)
    (syntax-rules . 2) (syntax-case . 2) (match-lambda . 2) (match . 2)
    (assert . 2) (test . 2) (test-assert . 2) (test-equal . 2)
    (test-error . 2) (test-group . 2)
    (assert-equal . 2) (assert-true . 2) (assert-false . 2)
    (assert-error . 2) (assert-no-error . 2) (assert-raises . 2)
    (assert-eq . 2) (assert-eqv . 2) (assert-contains . 2)
    (assert-matches . 2) (assert-empty . 2) (assert-in-range . 2)
    (assert-not-equal . 2) (assert-length . 2) (assert-one-of . 2)
    (suite . 2) (test-each . 2) (suite-each . 2) (scenario . 2)
    (given . 2) (when-step . 2) (then . 2) (and-step . 2) (but-step . 2)
    (while . 2) (benchmark . 2) (veritas-run . 2) (data-driven . 2)))

(define elisp-indent-rules
  '((defun . 2) (defmacro . 2) (defsubst . 2) (defalias . 2)
    (cl-defun . 2) (cl-defmacro . 2) (cl-defsubst . 2) (cl-defgeneric . 2)
    (cl-defmethod . 2) (cl-defstruct . 2) (cl-labels . 2) (cl-flet . 2)
    (defvar . 2) (defconst . 2) (defcustom . 2) (defface . 2) (defgroup . 2)
    (define-minor-mode . 2) (define-derived-mode . 2)
    (define-globalized-minor-mode . 2) (defun* . 2) (defadvice . 2)
    (advice-add . 2)
    (let . 2) (let* . 2) (letrec . 2) (if . 2) (if-let . 2) (when-let . 2)
    (cond . clauses) (case . 2) (pcase . clauses) (pcase-let . 2) (pcase-let* . 2)
    (when . 2) (unless . 2) (while . 2) (progn . 2) (prog1 . 2) (prog2 . 2)
    (condition-case . 2) (condition-case-unless-debug . 2) (ignore-errors . 2)
    (unwind-protect . 2) (catch . 2) (throw . 2) (signal . 2)
    (dolist . 2) (dotimes . 2) (loop . 2) (seq-let . 2) (seq-doseq . 2)
    (save-excursion . 2) (save-restriction . 2) (save-match-data . 2)
    (with-current-buffer . 2) (with-temp-buffer . 2) (with-temp-file . 2)
    (with-output-to-string . 2) (with-eval-after-load . 2)
    (with-silent-modifications . 2) (with-demoted-errors . 2)
    (lambda . 2) (quote . 2) (function . 2) (backquote . 2)
    (interactive . 2) (declare . 2) (require . 2) (provide . 2)
    (use-package . 2)
    (add-hook . 2) (remove-hook . 2) (setq . 2) (setq-default . 2)
    (ert-deftest . 2) (should . 2) (should-error . 2) (should-not . 2)
    (cl-letf . 2) (dolist-with-progress-reporter . 2)))

(define cl-indent-rules
  '((defun . 2) (defmacro . 2) (defmethod . 2) (defgeneric . 2)
    (defclass . 2) (defpackage . 2) (defstruct . 2) (define-condition . 2)
    (defparameter . 2) (defvar . 2) (defconstant . 2) (defsetf . 2)
    (define-symbol-macro . 2)
    (let . 2) (let* . 2) (flet . 2) (labels . 2) (macrolet . 2)
    (symbol-macrolet . 2) (multiple-value-bind . 2) (destructuring-bind . 2)
    (multiple-value-setq . 2) (multiple-value-prog1 . 2)
    (multiple-value-list . 2)
    (with-open-file . 2) (with-slots . 2) (with-accessors . 2)
    (with-output-to-string . 2) (with-input-from-string . 2)
    (with-package-iterator . 2) (with-standard-io-syntax . 2)
    (loop . 2) (do . 2) (do* . 2) (dolist . 2) (dotimes . 2)
    (cond . clauses) (case . 2) (ecase . 2) (ccase . 2) (typecase . 2)
    (etypecase . 2) (ctypecase . 2) (progn . 2) (prog1 . 2) (prog2 . 2)
    (prog . 2) (prog* . 2) (block . 2) (return-from . 2) (tagbody . 2)
    (catch . 2) (throw . 2) (unwind-protect . 2) (handler-case . 2)
    (handler-bind . 2) (restart-case . 2) (restart-bind . 2)
    (ignore-errors . 2)
    (when . 2) (unless . 2) (if . 2) (and . 2) (or . 2)
    (lambda . 2) (function . 2) (quote . 2) (setf . 2) (setq . 2)
    (push . 2) (pushnew . 2) (incf . 2) (decf . 2) (assert . 2)
    (check-type . 2) (eval-when . 2) (locally . 2) (declaim . 2)
    (declare . 2) (in-package . 2) (use-package . 2) (import . 2) (export . 2)))

(define (indent-rules dialect)
  "Return the indentation table for @var{dialect}."
  (case dialect
    [(scheme) scheme-indent-rules]
    [(elisp) elisp-indent-rules]
    [(cl) cl-indent-rules]
    [else scheme-indent-rules]))

(define* (indent-rule dialect operator #:optional (indent-width 2))
  "Return the indentation rule for @var{operator} in @var{dialect}.

@code{#f} means the form is not in the table and the generic rule applies.
The table is keyed by @code{(dialect, symbol)}, so `defun' can mean one thing
in Emacs Lisp and another in Common Lisp without either guessing."
  (let ((entry (assq operator (indent-rules dialect))))
    (and entry (cdr entry))))

;;; ===== Output tracking =====

(define-record-type <out>
  (%make-out port column line)
  out?
  (port out-port)
  (column out-column set-out-column!)
  (line out-line set-out-line!))

(define (make-out)
  (%make-out (open-output-string) 0 1))

(define (emit! out string)
  "Write @var{string}, tracking the column and line it ends on.

Both numbers matter: the column is what indentation and comment alignment
are computed from, and the line tells the dangling style whether a form was
written across several lines."
  (display string (out-port out))
  (string-for-each
   (lambda (c)
     (if (char=? c #\newline)
       (begin (set-out-line! out (+ 1 (out-line out)))
         (set-out-column! out 0))
       (set-out-column! out (+ 1 (out-column out)))))
   string))

(define (emit-spaces! out n)
  (when (> n 0) (emit! out (make-string n #\space))))

(define (emit-newlines! out n)
  (when (> n 0) (emit! out (make-string n #\newline))))

(define (out-text out)
  (get-output-string (out-port out)))

;;; ===== Rendering context =====

(define-record-type <ctx>
  (%make-ctx dialect style indent-width top? alignment marks brackets rules
             blank-lines)
  ctx?
  (dialect ctx-dialect)
  (style ctx-style)
  (indent-width ctx-indent-width)
  (top? ctx-top?) ; #t while rendering top-level nodes
  (alignment ctx-alignment) ; alist of comment index -> column
  (marks ctx-marks) ; mutable box of measurement triples
  (brackets ctx-brackets) ; square, parens or preserve
  (rules ctx-rules) ; the project's own (symbol . columns) rules
  (blank-lines ctx-blank-lines)) ; preserve or one

(define* (make-ctx #:key (dialect 'scheme) (style 'community)
                   (indent-width 2) (alignment '()) (marks #f) (brackets #f)
                   (rules '()) (blank-lines 'preserve))
  (%make-ctx dialect style indent-width #t alignment
             (or marks (cons '() '()))
             (or brackets (default-brackets dialect))
             rules
             (or blank-lines default-blank-lines)))

(define (ctx-inside ctx)
  (%make-ctx (ctx-dialect ctx) (ctx-style ctx) (ctx-indent-width ctx)
             #f (ctx-alignment ctx) (ctx-marks ctx) (ctx-brackets ctx)
             (ctx-rules ctx) (ctx-blank-lines ctx)))

(define (ctx-rule ctx head-name)
  "Return the project's own rule for @var{head-name}, or @code{#f}.

A project's rule wins over the built-in table: a macro called `with-foo'
means what the project says it means, and no formatter can know that from
the name."
  (let ((entry (assq head-name (ctx-rules ctx))))
    (and entry (cdr entry))))

(define (separate-top-level? ctx seen last)
  "Return @code{#t} when a top-level node must follow one blank line.

`blank-lines: one' is Riastradh's rule: adjacent top-level forms are
separated by exactly one blank line.  A heading comment belongs to the form
under it, so the gap after a form gets its blank line whether the next thing
is a form or the comment that heads it; a gap that follows a comment is the
author's grouping, and is left alone."
  (and (ctx-top? ctx)
    (eq? (ctx-blank-lines ctx) 'one)
    seen last
    (node-data? last)))

(define (newline-cap ctx)
  "Return the largest number of newlines a gap may keep.

At top level a run of blank lines is collapsed to one blank line; inside a
form two are allowed, because a blank line between two `let' bindings can be
the clearest comment there is.  A project that sets `blank-lines: one' gets
one blank line wherever it happens, which is the limit Riastradh's rules
argue for: two blank lines mean the same as one, at twice the cost."
  (cond
    [(ctx-top? ctx) 2]
    [(eq? (ctx-blank-lines ctx) 'one) 2]
    [else 3]))

(define (notes-box-push! box item)
  (set-cdr! box (cons item (cdr box))))

(define (alignment-column ctx node)
  (let ((entry (assv (source-location-index (node-start node))
                     (ctx-alignment ctx))))
    (and entry (cdr entry))))

;;; ===== The sequence renderer =====

(define* (render-sequence nodes indent out ctx #:optional (clause-style #f))
  "Render @var{nodes} at one nesting level.

Whitespace between nodes is not copied: it becomes one space when the nodes
share a line, and a newline plus @var{indent} when the author broke the line.
A comment is never moved to another line than the one it was on."
  (let loop ((rest nodes) (pending 0) (seen #f) (last #f))
    (match rest
      (()
       (when (and seen (> pending 0) (ctx-top? ctx))
         ;; Trailing whitespace at end of file: keep at most what the cap
         ;; allows, and never invent one.
         (emit-newlines! out (min pending (newline-cap ctx))))
       last)
      ((node . tail)
       (cond
         [(node-whitespace? node)
          (loop tail (+ pending (node-newlines node)) seen last)]

         [(node-comment? node)
          (cond
            [(node-inline? node)
             (when (> pending 0)
               (emit-newlines! out (min pending (newline-cap ctx)))
               (emit-spaces! out indent))
             (when seen
               (let ((target (alignment-column ctx node)))
                 (if (and target (> target (out-column out)))
                   (emit-spaces! out (- target (out-column out)))
                   (emit-spaces! out 1))))
             (notes-box-push! (ctx-marks ctx)
                              (list (source-location-index (node-start node))
                                    (out-line out)
                                    (out-column out)))
             (emit! out (node-value node))
             (loop tail 0 #t node)]
            [else
              (let ((separate? (separate-top-level? ctx seen last)))
                (cond
                  [(> pending 0)
                   (emit-newlines! out
                                   (if separate?
                                     2
                                     (min (max pending 1) (newline-cap ctx))))
                   (emit-spaces! out indent)]
                  [seen
                    ;; A standalone comment has to start a line of its own.
                    (emit-newlines! out (if separate? 2 1))
                    (emit-spaces! out indent)]))
              (emit! out (node-value node))
              (loop tail 0 #t node)])]

         [(node-stray-delimiter? node)
          ;; An unmatched closer is preserved next to whatever preceded it,
          ;; because it is an error the author has to see, not a form.
          (emit! out (node-value node))
          (loop tail 0 #t node)]

         [else
           ;; `blank-lines: one' is the one thing a formatter is allowed to
           ;; add: two adjacent top-level forms are separated by exactly one
           ;; blank line, whether or not the author wrote one.  A heading
           ;; comment above a form belongs to that form, so a gap that follows
           ;; a comment is left as it was.
           (let ((separate? (separate-top-level? ctx seen last)))
             (cond
               [(> pending 0)
                (emit-newlines! out
                                (if separate? 2 (min pending (newline-cap ctx))))
                (emit-spaces! out indent)]
               [separate? (emit-newlines! out 2)]
               [seen (emit-spaces! out 1)]))
           (render-node node out ctx clause-style)
           (loop tail 0 #t node)])))))

(define* (render-node node out ctx #:optional (clause-style #f))
  (cond
    [(node-verbatim? node)
     ;; A datum comment, a "#<...>" object or a "#." form is copied as it was
     ;; written: formatting *inside* something the reader was told not to enter
     ;; would be a guess, and sift does not guess.  What it does do is move the
     ;; block with the code around it: the whole text shifts by however much
     ;; its first line moved, so a multi-line `#;' keeps the shape its author
     ;; gave it instead of ending up half outside the form it belongs to.
     (emit-verbatim! out node)]
    [(node-list? node) (render-list node out ctx clause-style)]
    [(node-prefix? node) (render-prefix node out ctx)]
    [(node-conditional? node) (render-conditional node out ctx)]
    [else (emit! out (node-value node))]))

(define (emit-verbatim! out node)
  "Write a verbatim @var{node}, shifting its continuation lines with it.

The first line is written where the renderer has arrived; every later line
moves by the same number of columns, so the block keeps its internal shape.
A line that had less indentation than the shift removes is left at column
zero rather than pushed through it."
  (let* ((text (or (node-raw node) (node-value node)))
         (original (- (source-location-column (node-start node)) 1))
         (delta (- (out-column out) original)))
    (if (or (zero? delta) (not (string-index text #\newline)))
      (emit! out text)
      (emit! out (shift-block text delta)))))

(define (shift-block text delta)
  "Return @var{text} with its continuation lines moved by @var{delta} columns."
  (let ((lines (string-split text #\newline)))
    (string-join
     (cons (car lines)
           (map (lambda (line) (shift-line line delta)) (cdr lines)))
     "\n")))

(define (shift-line line delta)
  (cond
    [(zero? delta) line]
    [(> delta 0) (string-append (make-string delta #\space) line)]
    [else
      (let loop ((rest (string->list line)) (left (- delta)))
        (if (or (zero? left) (null? rest))
          (list->string rest)
          (if (memv (car rest) '(#\space #\tab))
            (loop (cdr rest) (- left 1))
            (list->string rest))))]))

(define (render-prefix node out ctx)
  (emit! out (node-token node))
  (let ((child (node-child node)))
    ;; The formatter clamps a prefix to its datum: no space, ever, even when
    ;; the source had one.  `reader-prefix-break' is the checker that reports
    ;; the style violation this fixes.
    (when child (render-node child out ctx))))

(define (render-conditional node out ctx)
  (emit! out (node-token node))
  (let ((feature (node-feature node))
        (guarded (node-guarded-form node)))
    (when feature (render-node feature out ctx))
    (when guarded
      (emit! out " ")
      (render-node guarded out ctx))))

(define* (render-list node out ctx #:optional (clause-style #f))
  (let* ((open-column (out-column out))
         (open-line (out-line out))
         (delimiters (delimiters-for node clause-style ctx))
         (open (car delimiters))
         (close (cdr delimiters))
         (children (node-children node)))
    (emit! out open)
    (let ((indent (list-indent node open-column ctx (and clause-style #t))))
      (render-sequence children indent out (ctx-inside ctx)
                       (clause-style-for node ctx)))
    (when close
      (emit-close node open-column open-line close out ctx clause-style))))

(define (delimiters-for node clause-style ctx)
  "Return the @code{(open . close)} pair @var{node} is written with.

A clause list of `cond' or `case' is written with the bracket style the
dialect asks for -- square in Scheme -- and every other list keeps the
delimiters it was read with, because `#(' is not a round bracket by another
name."
  (let ((open (node-open node))
        (close (node-close node)))
    (if (and clause-style (round-or-square? open close))
      (case clause-style
        [(square) (cons "[" "]")]
        [(parens) (cons "(" ")")]
        [else (cons open close)])
      (cons open close))))

(define (round-or-square? open close)
  (or (and (equal? open "(") (equal? close ")"))
    (and (equal? open "[") (equal? close "]"))
    ;; a list the reader could not close keeps its opener and gets no
    ;; closer, and must not be rewritten
    (and (equal? open "(") (not close))
    (and (equal? open "[") (not close))))

(define (clause-taking-forms dialect)
  "Return the forms of @var{dialect} whose children are clauses.

A clause is a test or a condition and a body, so the forms that take them are
the ones whose children are not arguments: aligning a body under the operator
of its clause -- `(error (e) ...' under `(e)' -- is not how either Lisp that
writes clauses is read."
  (case dialect
    [(elisp) '(cond pcase condition-case
                    cl-case cl-ecase cl-typecase cl-etypecase)]
    [(cl) '(cond case ecase ccase typecase etypecase ctypecase
                 handler-case restart-case)]
    [else '(cond case)]))

(define (clause-style-for node ctx)
  "Return how the children of @var{node} are written, when they are clauses.

@code{#f} means they are not clauses at all: they are arguments, and the
generic rule applies.  `square' or `parens' is the bracket a Scheme clause is
written with, and `keep' says the child is a clause that keeps its own
delimiters -- which is what a dialect with no such convention gets.

Only Scheme brackets its clauses.  A square bracket is not Emacs Lisp syntax,
and is not Common Lisp syntax either, so `brackets: square' in a project's
configuration -- which reads as a choice about style -- would otherwise write
a file its own dialect cannot read back, and a formatter that does that is
worse than one with no opinion.

A vector is never a clause list, whatever its first element is called: the
`cond' in `#(cond a b)' is a symbol inside a literal, not the form, and
@code{node-head-name} answers @code{#f} for it."
  (let ((head (node-head-name node))
        (brackets (ctx-brackets ctx)))
    (and (memq head (clause-taking-forms (ctx-dialect ctx)))
      (if (and (eq? (ctx-dialect ctx) 'scheme)
            (memq brackets '(square parens)))
        brackets
        'keep))))

(define* (emit-close node open-column open-line close out ctx
                     #:optional (clause-style #f))
  (cond
    [(and (eq? (ctx-style ctx) 'dangling)
       (> (out-line out) open-line))
     ;; The closing delimiter goes on a line of its own, aligned with the
     ;; opener -- immediately after the last element, never after a blank
     ;; line.
     (emit! out "\n")
     (emit-spaces! out open-column)
     (emit! out close)]
    [(eq? (ctx-style ctx) 'dangling)
     (emit! out close)]
    [else
      ;; Community style: the closer trails the last element.  When the last
      ;; thing written was a comment, the closer cannot share its line, so it
      ;; goes on the next one at the form's indentation.
      (let ((last (last-written-child (node-children node))))
        (when (and last (node-comment? last))
          (emit! out "\n")
          (emit-spaces! out (list-indent node open-column ctx
                                         (and clause-style #t)))))
      (emit! out close)]))

(define (last-written-child children)
  (let loop ((rest (reverse children)))
    (cond [(null? rest) #f]
          [(node-whitespace? (car rest)) (loop (cdr rest))]
          [else (car rest)])))

;;; ===== Indentation =====

(define* (list-indent node open-column ctx #:optional (clause? #f))
  "Return the column a child of @var{node} starts at when it starts a line.

@var{clause?} says @var{node} is a clause of a form that takes them, where
the first element names a condition and what follows is a body."
  (let* ((open (node-open node))
         (open-width (string-length open))
         (elements (node-elements node))
         (head-name (node-head-name node))
         (width (ctx-indent-width ctx)))
    (cond
      [(null? elements) (+ open-column open-width)]
      [(or (not head-name) (node-vector? node) (node-bytevector? node))
       ;; A data list -- a binding list, a `cond' clause, a quoted structure --
       ;; lines its elements up under the first.  So does a vector: `#(foo
       ;; bar)' holds two symbols and is not a call to `foo', so its elements
       ;; line up under the first one even when that one is a symbol.
       (+ open-column open-width)]
      [else
        (let ((rule (or (ctx-rule ctx head-name)
                      (indent-rule (ctx-dialect ctx) head-name width))))
          (cond
            [(eq? rule 'data) (+ open-column open-width)]
            [(eq? rule 'clauses)
             ;; A clause list belongs to its form the way a data list belongs
             ;; to its opener: when the first clause is on the head line the
             ;; rest line up under it, which is how `cond' and `case' are read
             ;; in every Lisp.  With no clause on the head line the rule is
             ;; the ordinary two columns.
             (if (first-argument-on-head-line? node)
               (+ open-column open-width
                  (string-length (node-value (node-head node)))
                  1)
               (+ open-column (ctx-indent-width ctx)))]
            [(integer? rule) (+ open-column rule)]
            ;; A clause of `handler-case' or `condition-case' is not a call:
            ;; its first element names the condition (or the pattern) and the
            ;; rest is a body, so the body indents two columns like every
            ;; other body.  Lining it up under the condition would push it
            ;; into the middle of the line, which is not how either dialect
            ;; that writes clauses is read.
            [clause? (+ open-column width)]
            [else
              ;; The generic rule, which is what every Lisp editor does with
              ;; a function call: the arguments line up under the first one,
              ;; so a list of them is a column of names rather than a
              ;; staircase,
              ;;
              ;;   (list first
              ;;         second)
              ;;
              ;; and a call with keyword arguments gets the same alignment
              ;; without a rule of its own.
              (if (and (head-on-open-line? node)
                    (first-argument-on-head-line? node))
                (alignment-column-for node open-column)
                ;; With nothing but the operator on its line there is
                ;; nothing to line up under, so the rest of the form goes one
                ;; column in from the opener: the position Emacs's own
                ;; indentation gives it.
                (+ open-column open-width))]))])))

(define (alignment-column-for node open-column)
  "Return the column of the first argument of @var{node}.

`(make-window title' puts `title' at the opener's column plus the opener,
the operator and one space; that is where the rest of the form belongs, and
where the keywords of a call belong for the same reason."
  (+ open-column
     (string-length (node-open node))
     (string-length (node-value (node-head node)))
     1))

(define (head-on-open-line? node)
  "Return @code{#t} when the first element of @var{node} is on the opener's line.

An alignment column is counted from the opener, so it only means anything when
the head it counts past is on that same line.  In

  '(
    insert-tab)

the head is on the line below the opener, and lining the rest of the form up
under an operator that starts one column in would push it fifteen columns to
the right for no reason the author can see."
  (let loop ((rest (node-children node)))
    (cond
      [(null? rest) #f]
      [(node-whitespace? (car rest))
       (and (zero? (node-newlines (car rest))) (loop (cdr rest)))]
      [(node-comment? (car rest)) (loop (cdr rest))]
      [else #t])))

(define (first-argument-on-head-line? node)
  "Return @code{#t} when the second element of @var{node} is on the head's line."
  (let loop ((rest (node-children node)) (seen-head? #f))
    (cond
      [(null? rest) #t]
      [(node-whitespace? (car rest))
       (if (and seen-head? (> (node-newlines (car rest)) 0))
         #f
         (loop (cdr rest) seen-head?))]
      [(node-comment? (car rest)) (loop (cdr rest) seen-head?)]
      [seen-head? #t]
      [else (loop (cdr rest) #t)])))

;;; ===== Comment alignment =====

(define (compute-alignment marks)
  "Turn the measurement pass into a table of comment index to target column.

Two comments belong to the same column group when they are on consecutive
lines, which is what \"contiguous\" means in a source file.  Each group is
aligned one column past the longest line of code in it."
  (define (flush group out)
    (if (null? group)
      out
      (let ((target (apply max (map (lambda (mark) (list-ref mark 2)) group))))
        (append out
                (map (lambda (mark) (cons (car mark) target)) group)))))
  (let loop ((rest marks) (group '()) (out '()))
    (cond
      [(null? rest) (flush group out)]
      [(and (pair? group)
         (= (list-ref (car rest) 1) (+ 1 (list-ref (car group) 1))))
       (loop (cdr rest) (cons (car rest) group) out)]
      [else (loop (cdr rest) (list (car rest)) (flush group out))])))

;;; ===== Whole-document rendering =====

(define (render-document ast ctx)
  "Render @var{ast}, returning the text and the inline-comment measurements."
  (let* ((out (make-out))
         (top (%make-ctx (ctx-dialect ctx) (ctx-style ctx)
                         (ctx-indent-width ctx) #t (ctx-alignment ctx)
                         (ctx-marks ctx) (ctx-brackets ctx) (ctx-rules ctx)
                         (ctx-blank-lines ctx))))
    (render-sequence ast 0 out top)
    (values (out-text out) (reverse (cdr (ctx-marks top))))))

(define* (format-ast ast #:key (dialect 'scheme) (style 'community)
                     (indent-width 2) (comment-align #f) (brackets #f)
                     (indent-rules '()) (blank-lines 'preserve))
  "Render @var{ast} as source text.

The result is deterministic: nothing here depends on a hash table's order or
on any state outside the arguments."
  (let* ((base (make-ctx #:dialect dialect #:style style
                         #:indent-width indent-width #:brackets brackets
                         #:rules indent-rules #:blank-lines blank-lines)))
    (if comment-align
      (call-with-values
        (lambda () (render-document ast base))
        (lambda (text marks)
          (let* ((table (compute-alignment marks))
                 (aligned (%make-ctx dialect style indent-width #t table
                                     (cons '() '()) (ctx-brackets base)
                                     (ctx-rules base) (ctx-blank-lines base))))
            (call-with-values
              (lambda () (render-document ast aligned))
              (lambda (aligned-text ignored) aligned-text)))))
      (call-with-values (lambda () (render-document ast base))
        (lambda (text marks) text)))))

;;; ===== Entry points =====

(define* (format-source source #:key (dialect 'scheme) (file #f)
                        (style 'community) (indent #f) (comment-align #f)
                        (line-length #f) (force #f) (brackets #f)
                        (indent-rules '()) (blank-lines 'preserve))
  "Format @var{source}, returning two values: the text and the parse errors.

No file is read from or written to here, and no error is swallowed: the
caller decides whether an error is fatal (see @code{format/file})."
  (call-with-values
    (lambda () (read-source #:source source #:dialect dialect #:file file))
    (lambda (ast errors)
      (values (format-ast ast #:dialect dialect #:style style
                          #:indent-width (or indent (default-indent dialect))
                          #:comment-align comment-align
                          #:brackets brackets
                          #:indent-rules indent-rules
                          #:blank-lines blank-lines)
              errors))))

(define* (format-string source #:key (dialect 'scheme) (style 'community)
                        (indent 2) (comment-align #f) (brackets #f)
                        (indent-rules '()) (blank-lines 'preserve))
  "Format @var{source} and return just the text."
  (call-with-values
    (lambda () (read-source #:source source #:dialect dialect))
    (lambda (ast errors)
      (format-ast ast #:dialect dialect #:style style
                  #:indent-width indent #:comment-align comment-align
                  #:brackets brackets #:indent-rules indent-rules
                  #:blank-lines blank-lines))))

(define* (format/file path #:key (dialect #f) (style 'community) (indent 2)
                      (comment-align #f) (force #f) (brackets #f)
                      (indent-rules '()) (blank-lines 'preserve))
  "Format the file at @var{path}, returning the text and the parse errors.

The dialect is detected from @var{path} when @var{dialect} is @code{#f}."
  (let* ((source (call-with-input-file path get-string-all))
         (dialect* (or dialect
                     (detect-dialect #:file path #:source source)
                     'scheme)))
    (format-source source #:dialect dialect* #:file path #:style style
                   #:indent indent #:comment-align comment-align
                   #:force force #:brackets brackets #:indent-rules indent-rules
                   #:blank-lines blank-lines)))

(define* (format-check source #:key (dialect 'scheme) (style 'community)
                       (indent 2) (comment-align #f) (brackets #f)
                       (blank-lines 'preserve))
  "Return @code{#t} when formatting @var{source} would change it.

This is what `sift check' asks: not \"is the file ugly?\" but \"would running
the formatter change a single byte?\"."
  (call-with-values
    (lambda () (format-source source #:dialect dialect #:style style
                              #:indent indent #:comment-align comment-align
                              #:blank-lines blank-lines))
    (lambda (formatted errors)
      (not (string=? formatted source)))))

(define (significant-nodes ast)
  "Return the nodes of @var{ast} that are not whitespace and not a comment."
  (filter node-data? ast))
