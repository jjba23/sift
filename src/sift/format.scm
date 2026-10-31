;; sift --- formatter
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

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
Scheme and Common Lisp get 100."
  (if (eq? dialect 'elisp) 80 100))

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

(define scheme-indent-rules
  '((lambda . 2) (lambda* . 2) (case-lambda . 2)
    (define . 2) (define* . 2) (define-public . 2) (define-syntax . 2)
    (define-syntax-rule . 2) (define-record-type . 2) (define-values . 2)
    (define-module . 2) (define-class . 2) (define-generic . 2)
    (define-method . 2) (define-inlinable . 2) (define-constant . 2)
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
  (%make-ctx dialect style indent-width top? alignment marks brackets rules)
  ctx?
  (dialect ctx-dialect)
  (style ctx-style)
  (indent-width ctx-indent-width)
  (top? ctx-top?) ; #t while rendering top-level nodes
  (alignment ctx-alignment) ; alist of comment index -> column
  (marks ctx-marks) ; mutable box of measurement triples
  (brackets ctx-brackets) ; square, parens or preserve
  (rules ctx-rules)) ; the project's own (symbol . columns) rules

(define* (make-ctx #:key (dialect 'scheme) (style 'community)
           (indent-width 2) (alignment '()) (marks #f) (brackets #f)
           (rules '()))
  (%make-ctx dialect style indent-width #t alignment
    (or marks (cons '() '()))
    (or brackets (default-brackets dialect))
    rules))

(define (ctx-inside ctx)
  (%make-ctx (ctx-dialect ctx) (ctx-style ctx) (ctx-indent-width ctx)
    #f (ctx-alignment ctx) (ctx-marks ctx) (ctx-brackets ctx)
    (ctx-rules ctx)))

(define (ctx-rule ctx head-name)
  "Return the project's own rule for @var{head-name}, or @code{#f}.

A project's rule wins over the built-in table: a macro called `with-foo'
means what the project says it means, and no formatter can know that from
the name."
  (let ((entry (assq head-name (ctx-rules ctx))))
    (and entry (cdr entry))))

(define (newline-cap ctx)
  "Return the largest number of newlines a gap may keep.

At top level a run of blank lines is collapsed to one blank line; inside a
form two are allowed, because a blank line between two `let' bindings can be
the clearest comment there is."
  (if (ctx-top? ctx) 2 3))

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
               (cond
                 [(> pending 0)
                  (emit-newlines! out (min (max pending 1) (newline-cap ctx)))
                  (emit-spaces! out indent)]
                 [seen
                    ;; A standalone comment has to start a line of its own.
                    (emit-newlines! out 1)
                    (emit-spaces! out indent)])
               (emit! out (node-value node))
               (loop tail 0 #t node)])]

         [(node-stray-delimiter? node)
          ;; An unmatched closer is preserved next to whatever preceded it,
          ;; because it is an error the author has to see, not a form.
          (emit! out (node-value node))
          (loop tail 0 #t node)]

         [else
            (cond
              [(> pending 0)
               (emit-newlines! out (min pending (newline-cap ctx)))
               (emit-spaces! out indent)]
              [seen (emit-spaces! out 1)])
            (render-node node out ctx clause-style)
            (loop tail 0 #t node)])))))

(define* (render-node node out ctx #:optional (clause-style #f))
  (cond
    [(node-verbatim? node)
     ;; A datum comment, a "#<...>" object or a "#." form is copied exactly as
     ;; it was written: formatting inside something the reader was told not to
     ;; enter would be a guess, and sift does not guess.
     (emit! out (or (node-raw node) (node-value node)))]
    [(node-list? node) (render-list node out ctx clause-style)]
    [(node-prefix? node) (render-prefix node out ctx)]
    [(node-conditional? node) (render-conditional node out ctx)]
    [else (emit! out (node-value node))]))

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
    (let ((indent (list-indent node open-column ctx)))
      (render-sequence children indent out (ctx-inside ctx)
        (clause-brackets-for node ctx)))
    (when close
      (emit-close node open-column open-line close out ctx))))

(define (delimiters-for node clause-style ctx)
  "Return the @code{(open . close)} pair @var{node} is written with.

A clause list of `cond' or `case' is written with the bracket style the
dialect asks for -- square in Scheme -- and every other list keeps the
delimiters it was read with, because `#(' and `{' are not round brackets by
another name."
  (let ((open (node-open node))
        (close (node-close node))
        (style (or clause-style
                 (and (eq? (node-kind node) 'list) #f))))
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

(define (clause-brackets-for node ctx)
  "Return the bracket style the children of @var{node} are written with.

Only the forms whose children *are* clauses take one: a `cond' clause is a
test and a body, and a `case' clause is a datum list and a body, so the
bracket tells the reader where one ends.  In every other list the parentheses
are structure, and the author keeps them."
  (let ((style (ctx-brackets ctx))
        (head (node-head-name node)))
    (and (memq style '(square parens))
      (memq head '(cond case))
      style)))

(define (emit-close node open-column open-line close out ctx)
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
           (emit-spaces! out (list-indent node open-column ctx))))
       (emit! out close)]))

(define (last-written-child children)
  (let loop ((rest (reverse children)))
    (cond [(null? rest) #f]
          [(node-whitespace? (car rest)) (loop (cdr rest))]
          [else (car rest)])))

;;; ===== Indentation =====

(define (list-indent node open-column ctx)
  "Return the column a child of @var{node} starts at when it starts a line."
  (let* ((open (node-open node))
         (open-width (string-length open))
         (elements (node-elements node))
         (head-name (node-head-name node))
         (width (ctx-indent-width ctx)))
    (cond
      [(null? elements) (+ open-column open-width)]
      [(not head-name)
       ;; A data list -- a binding list, a `cond' clause, a vector, a quoted
       ;; structure -- lines its elements up under the first.
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
             [else
                (cond
                  ;; A call with keyword arguments lines them up under the
                  ;; first argument, which is where the reader looks for the
                  ;; next keyword: the keywords become a column of names and
                  ;; values rather than a staircase.
                  [(keyword-arguments? node)
                   (alignment-column-for node open-column)]
                  ;; The generic rule: arguments sit two columns in from the
                  ;; opener when the first one is on the operator's line, and
                  ;; two columns in from the operator when the author moved it
                  ;; down.
                  [(first-argument-on-head-line? node) (+ open-column width)]
                  [else (+ open-column open-width width)])]))])))

(define (keyword-arguments? node)
  "Return @code{#t} when @var{node} is a call with keyword arguments.

Only a call: a data list of keywords is a plist someone is building, and it
is already aligned by the data rule."
  (let ((elements (node-elements node)))
    (and (node-head-name node)
      (pair? elements)
      (any (lambda (element)
             (and (node-atom? element) (eq? (node-atom-type element) 'keyword)))
        (cdr elements)))))

(define (alignment-column-for node open-column)
  "Return the column of the first argument of @var{node}.

`(make-window title' puts `title' at the opener's column plus the opener,
the operator and one space; that is where every keyword below it belongs."
  (+ open-column
    (string-length (node-open node))
    (string-length (node-value (node-head node)))
    1))

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
                (ctx-marks ctx) (ctx-brackets ctx) (ctx-rules ctx))))
    (render-sequence ast 0 out top)
    (values (out-text out) (reverse (cdr (ctx-marks top))))))

(define* (format-ast ast #:key (dialect 'scheme) (style 'community)
           (indent-width 2) (comment-align #f) (brackets #f)
           (indent-rules '()))
  "Render @var{ast} as source text.

The result is deterministic: nothing here depends on a hash table's order or
on any state outside the arguments."
  (let* ((base (make-ctx #:dialect dialect #:style style
                 #:indent-width indent-width #:brackets brackets
                 #:rules indent-rules)))
    (if comment-align
      (call-with-values
        (lambda () (render-document ast base))
        (lambda (text marks)
          (let* ((table (compute-alignment marks))
                 (aligned (%make-ctx dialect style indent-width #t table
                            (cons '() '()) (ctx-brackets base)
                            (ctx-rules base))))
            (call-with-values
              (lambda () (render-document ast aligned))
              (lambda (aligned-text ignored) aligned-text)))))
      (call-with-values (lambda () (render-document ast base))
        (lambda (text marks) text)))))

;;; ===== Entry points =====

(define* (format-source source #:key (dialect 'scheme) (file #f)
           (style 'community) (indent #f) (comment-align #f)
           (line-length #f) (force #f) (brackets #f) (indent-rules '()))
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
                #:indent-rules indent-rules)
        errors))))

(define* (format-string source #:key (dialect 'scheme) (style 'community)
           (indent 2) (comment-align #f) (brackets #f) (indent-rules '()))
  "Format @var{source} and return just the text."
  (call-with-values
    (lambda () (read-source #:source source #:dialect dialect))
    (lambda (ast errors)
      (format-ast ast #:dialect dialect #:style style
        #:indent-width indent #:comment-align comment-align
        #:brackets brackets #:indent-rules indent-rules))))

(define* (format/file path #:key (dialect #f) (style 'community) (indent 2)
           (comment-align #f) (force #f) (brackets #f) (indent-rules '()))
  "Format the file at @var{path}, returning the text and the parse errors.

The dialect is detected from @var{path} when @var{dialect} is @code{#f}."
  (let* ((source (call-with-input-file path get-string-all))
         (dialect* (or dialect
                     (detect-dialect #:file path #:source source)
                     'scheme)))
    (format-source source #:dialect dialect* #:file path #:style style
      #:indent indent #:comment-align comment-align
      #:force force #:brackets brackets #:indent-rules indent-rules)))

(define* (format-check source #:key (dialect 'scheme) (style 'community)
           (indent 2) (comment-align #f) (brackets #f))
  "Return @code{#t} when formatting @var{source} would change it.

This is what `sift check' asks: not \"is the file ugly?\" but \"would running
the formatter change a single byte?\"."
  (call-with-values
    (lambda () (format-source source #:dialect dialect #:style style
                 #:indent indent #:comment-align comment-align))
    (lambda (formatted errors)
      (not (string=? formatted source)))))

(define (significant-nodes ast)
  "Return the nodes of @var{ast} that are not whitespace and not a comment."
  (filter node-data? ast))
