;; sift --- checker support
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Every checker is a procedure of @code{(ast source dialect)} that returns a
;; list of warnings and never mutates the tree.  This module holds what all of
;; them need to do that: a walk over the data nodes that stays out of datum
;; comments and opaque objects, the handful of accessors that read a form as a
;; program rather than as text, and the line arithmetic that the checkers
;; working on raw text rather than on forms need.

(define-module (sift check base)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (sift records)
  #:re-export (node-data? node-elements node-head node-head-name
                          node-inline? node-newlines node-child node-children
                          node-token node-open node-close node-value node-kind
                          node-start node-end node-atom-type node-package-marker
                          source-location-index source-location-line
                          source-location-column source-location-file
                          make-lint-warning make-lint-warning/range)
  #:export (for-each-node
            for-each-form
            for-each-top-level
            fold-nodes
            node-source
            quoted-context?
            in-formals?
            code-position?
            case-datum?
            case-form-heads
            list-headed-by?
            head-is-one-of?
            form-elements
            form-arguments
            argument-count
            atom-symbol
            symbol-atom?
            keyword-atom?
            string-atom?
            docstring-node
            body-elements
            walk-with-ancestors
            line-spans
            line-text
            indent-width-of
            node-column
            node-index
            node-end-index
            line-of-index
            location-at-index
            ranges-of
            index-in-ranges?
            blank-line?
            text-ranges-of
            current-check-options
            with-check-options
            option
            checker
            make-checker
            diagnose))

;;; ===== Walking =====

(define (for-each-node proc ast)
  "Call @var{proc} on every datum in @var{ast}, outermost first.

The walk does not enter a datum comment, an opaque object or a form that is
commented out: the linter has no business reporting on code the reader was
told not to read as code."
  (define (walk node)
    (unless (node-commented-out? node)
      (proc node)
      (cond
        [(node-list? node)
         (for-each walk (node-elements node))]
        [(node-prefix? node)
         (let ((child (node-child node)))
           (when (and child (not (node-datum-comment? node)))
             (walk child)))]
        [(node-conditional? node)
         (let ((feature (node-feature node))
               (guarded (node-guarded-form node)))
           (when feature (walk feature))
           (when guarded (walk guarded)))])))
  (for-each (lambda (node) (when (node-data? node) (walk node))) ast))

(define (for-each-top-level proc ast)
  "Call @var{proc} on the data nodes of @var{ast} that are not inside anything."
  (for-each (lambda (node) (when (node-data? node) (proc node))) ast))

(define (for-each-form proc ast)
  "Call @var{proc} on every node that is a form, outermost first.

A *form* is a node in a position where code is evaluated.  Three places are
not: anything inside a quote, the datum of a `case` clause, and a parameter
list.  A rule about the shape of a form asks this walker and never has to ask
whether what it is looking at is code at all -- which is what stops
`(if . 2)' in an alist from being reported as an `if` with no else branch."
  (walk-with-ancestors
   (lambda (node ancestors)
     (when (and (not (node-trivia? node))
             (code-position? node ancestors))
       (proc node)))
   ast))

(define (fold-nodes kons knil ast)
  "Fold @var{kons} over every datum in @var{ast}."
  (let ((acc knil))
    (for-each-node (lambda (node) (set! acc (kons node acc))) ast)
    acc))

(define (walk-with-ancestors proc ast)
  "Call @var{proc} with each datum and the list of forms that enclose it.

The ancestors come nearest-first, which is the order a checker asks them in:
\"is there a `let' between me and that `lambda'?\""
  (define (walk node ancestors)
    (unless (node-commented-out? node)
      (proc node ancestors)
      (cond
        [(node-list? node)
         (for-each (lambda (child) (walk child (cons node ancestors)))
                   (node-elements node))]
        [(node-prefix? node)
         ;; a prefix is a parent like any other: a checker asking whether a
         ;; node is inside a quote has to see the quote mark it is inside of
         (let ((child (node-child node)))
           (when (and child (not (node-datum-comment? node)))
             (walk child (cons node ancestors))))]
        [(node-conditional? node)
         (let ((feature (node-feature node))
               (guarded (node-guarded-form node)))
           (when feature (walk feature ancestors))
           (when guarded (walk guarded ancestors)))])))
  (for-each (lambda (node) (when (node-data? node) (walk node '()))) ast))

;;; ===== Reading a form =====

;; Forms whose clauses start with a datum rather than with code.  In
;; `(case x ((a b) ...))' the list `(a b)' is a list of *values* to match, not
;; a call, so a rule about the shape of a form has no business looking inside
;; it.
(define case-form-heads
  '(case ecase ccase typecase etypecase ctypecase))

(define (case-datum? node ancestors)
  "Return @code{#t} when @var{node} is the datum head of a `case` clause.

The walk reaches `(cond)' in `(case x [(cond) ...])' as if it were a `cond`
form; it is a datum, and no rule should treat it as code."
  (and (pair? ancestors)
    (pair? (cdr ancestors))
    (let ((clause (car ancestors))
          (form (cadr ancestors)))
      (and (node-list? clause)
        (node-list? form)
        (memq (node-head-name form) case-form-heads)
        (eq? (node-head clause) node)))))

(define (quoted-context? ancestors)
  "Return @code{#t} when the innermost quoted form is still in effect.

Walking up from a node, the first ancestor that is a quote, a quasiquote or a
`quote` form decides: inside it the node is data, and a list that happens to
start with `cond` is not a `cond` form."
  (let loop ((rest ancestors))
    (cond
      [(null? rest) #f]
      [(and (node-prefix? (car rest))
         (member (node-token (car rest)) '("'" "`")))
       #t]
      [(and (node-list? (car rest))
         (memq (node-head-name (car rest)) '(quote quasiquote)))
       #t]
      [(and (node-prefix? (car rest))
         (member (node-token (car rest)) '("," ",@")))
       ;; an unquote ends the data: what is inside it is evaluated again
       (loop (cdr rest))]
      [else (loop (cdr rest))])))

(define (in-formals? node ancestors)
  "Return @code{#t} when @var{node} sits inside a parameter list.

`(define* (f (labels #f)) ...)' has a pair in its formals that looks exactly
like a `labels` form; it is a parameter with a default, and no rule about
bodies has any business reading it as code."
  (let loop ((rest ancestors))
    (cond
      [(null? rest) #f]
      [(and (node-list? (car rest))
         (memq (node-head-name (car rest))
               '(lambda define define* define-public defun defmacro defsubst
                  cl-defun cl-defmacro defmethod defgeneric)))
       (let* ((elements (node-elements (car rest)))
              (formals
               (and (pair? elements)
                 (pair? (cdr elements))
                 (cadr elements))))
         (and formals
           (<= (node-index formals) (node-index node))
           (<= (node-end-index node) (node-end-index formals))))]
      [else (loop (cdr rest))])))

(define (code-position? node ancestors)
  "Return @code{#t} when a rule may read @var{node} as a form.

Three places are data rather than code: anything inside a quote, the datum of
a `case` clause, and a parameter list.  Every rule that asks about the shape
of a form asks this first."
  (not (or (quoted-context? ancestors)
         (case-datum? node ancestors)
         (in-formals? node ancestors))))

(define (node-source source node)
  "Return the source text @var{node} was read from."
  (substring source (node-index node) (node-end-index node)))

(define (list-headed-by? node name)
  "Return @code{#t} when @var{node} is a list whose operator is @var{name}."
  (and (node-list? node)
    (eq? (node-head-name node) name)))

(define (head-is-one-of? node names)
  "Return the operator of @var{node} when it is one of @var{names}, else #f."
  (let ((head (and (node-list? node) (node-head-name node))))
    (and head (memq head names) head)))

(define (form-elements node)
  "Return the data elements of @var{node}; empty when it is not a list."
  (if (node-list? node) (node-elements node) '()))

(define (form-arguments node)
  "Return the elements of @var{node} after its first one."
  (let ((elements (form-elements node)))
    (if (null? elements) '() (cdr elements))))

(define (argument-count node)
  "Return how many arguments (everything after the operator) @var{node} has."
  (length (form-arguments node)))

(define (atom-symbol node)
  "Return the name of an atom as a symbol, or @code{#f}.

A keyword is returned with its colon dropped, so that `:export' and `export'
compare the same way: a checker asking whether a clause is an export clause
should not have to know which spelling the author used."
  (and node
    (node-atom? node)
    (let ((value (node-value node))
          (type (node-atom-type node))
          (marker (node-package-marker node)))
      (cond
        [(eq? type 'keyword)
         (string->symbol (if (string-prefix? ":" value)
                           (substring value 1)
                           value))]
        ;; `#:run' and `app:run' name the symbol `run'; the marker says
        ;; which package it belongs to, not what it is called.
        [(and marker (< (string-length marker) (string-length value)))
         (string->symbol (substring value (string-length marker)))]
        [(eq? type 'symbol) (string->symbol value)]
        [else #f]))))

(define (symbol-atom? node name)
  "Return @code{#t} when @var{node} is the symbol @var{name}."
  (and node
    (node-atom? node)
    (eq? (node-atom-type node) 'symbol)
    (string=? (node-value node) (symbol->string name))))

(define (keyword-atom? node name)
  "Return @code{#t} when @var{node} is the keyword @code{:name}."
  (and node
    (node-atom? node)
    (eq? (node-atom-type node) 'keyword)
    (string=? (node-value node) (string-append ":" (symbol->string name)))))

(define (string-atom? node)
  (and node (node-atom? node) (eq? (node-atom-type node) 'string)))

(define (docstring-node node)
  "Return the docstring of @var{node}, the way each dialect spells it.

A function form (Scheme `define' with a head, Emacs Lisp `defun', Common Lisp
`defun') keeps its docstring after the argument list; a variable form after
the value.  A docstring is a string literal, and it is the only string that
is not a value here."
  (let* ((head (and (node-list? node) (node-head-name node)))
         (args (form-arguments node)))
    (define (after k)
      (if (<= k (length args))
        (let ((rest (list-tail args k)))
          (and (pair? rest) (string-atom? (car rest)) (car rest)))
        #f))
    (case head
      [(define define* define-public)
       (let ((first (and (pair? args) (car args))))
         ;; (define (f a) "doc" ...) versus (define x "value")
         (if (and first (node-list? first)) (after 1) #f))]
      [(defun defmacro defsubst defmethod cl-defun cl-defmacro cl-defsubst
              defgeneric)
       (after 2)]
      [(lambda lambda* cl-lambda)
       (after 1)]
      [(defvar defconst defcustom defparameter defface)
       (after 2)]
      [else #f])))

(define (body-elements node skip)
  "Return the body of @var{node}: its elements after the first @var{skip}.

A docstring is not a body form, so it is dropped when it comes first.  A
form shorter than @var{skip} has no body at all, and asking for one is not an
error: `(defun f ())` defines a function with no body, which is a different
problem from a crash."
  (let* ((elements (form-elements node))
         (rest (drop elements (min skip (length elements)))))
    (if (and (pair? rest) (string-atom? (car rest)))
      (cdr rest)
      rest)))

;;; ===== Lines and text =====

(define (line-spans source)
  "Return @code{(start . end)} character offsets for every line.

@var{end} excludes the line break, so a slice of the source is exactly the
line the user sees.  A file that ends with a newline has a final empty line,
which is what a text editor shows too."
  (let ((n (string-length source)))
    (let loop ((start 0) (i 0) (out '()))
      (cond
        [(>= i n)
         (reverse (cons (cons start i) out))]
        [(char=? (string-ref source i) #\newline)
         (loop (+ i 1) (+ i 1) (cons (cons start i) out))]
        [else (loop start (+ i 1) out)]))))

(define* (line-text source line #:optional (lines #f))
  "Return line @var{line} (1-based) of @var{source}, without its break."
  (let ((spans (or lines (line-spans source))))
    (and (>= line 1) (<= line (length spans))
      (let ((span (list-ref spans (- line 1))))
        (substring source (car span) (cdr span))))))

(define (indent-width-of source start end)
  "Return how many columns the whitespace from @var{start} to @var{end} spans.

A tab stops at the next multiple of eight, which is the convention every
editor uses for a file it has not been told about."
  (let loop ((i start) (column 0))
    (if (>= i end)
      column
      (loop (+ i 1)
            (if (char=? (string-ref source i) #\tab)
              (* 8 (+ 1 (quotient column 8)))
              (+ column 1))))))

(define (node-column node)
  (source-location-column (node-start node)))

(define (node-index node)
  (source-location-index (node-start node)))

(define (node-end-index node)
  (source-location-index (node-end node)))

(define (line-of-index source index)
  "Return the 1-based line of character @var{index} in @var{source}."
  (let loop ((spans (line-spans source)) (n 1))
    (cond [(null? spans) n]
          [(<= index (cdr (car spans))) n]
          [else (loop (cdr spans) (+ n 1))])))

(define (location-at-index source index)
  "Build a location from an index alone.

A checker is handed the AST and the text, not the file name, so the file is
left @code{#f} here and the driver fills it in from the run's context.  The
line and column are counted in characters, exactly as the reader counts
them."
  (let loop ((spans (line-spans source)) (number 1))
    (if (null? spans)
      (make-source-location #f number 1 index index)
      (let ((span (car spans)))
        (if (<= index (cdr span))
          (make-source-location #f number (+ 1 (- index (car span)))
                                index index)
          (loop (cdr spans) (+ number 1)))))))

(define (blank-line? text)
  "Return @code{#t} when @var{text} holds nothing but whitespace."
  (string-every (lambda (c) (memv c '(#\space #\tab #\return))) text))

;;; ===== Ranges =====

(define (ranges-of ast predicate)
  "Return the source ranges of every node in @var{ast} satisfying @var{predicate}."
  (reverse
   (fold-nodes (lambda (node acc)
                 (if (predicate node)
                   (cons (cons (node-index node) (node-end-index node)) acc)
                   acc))
               '() ast)))

(define (text-ranges-of ast)
  "Return the ranges in which source text is not code.

Strings and block comments are what a text-level checker must not look
inside: a tab in a string literal is data, and a line that starts inside a
multi-line block comment is not indentation."
  (append (ranges-of ast (lambda (node) (and (node-atom? node)
                                          (eq? (node-atom-type node) 'string))))
          (ranges-of ast (lambda (node)
                           (or (node-block-comment? node)
                             (node-string? node))))))

(define (index-in-ranges? index ranges)
  "Return @code{#t} when @var{index} falls inside one of @var{ranges}."
  (any (lambda (range) (and (>= index (car range)) (< index (cdr range))))
       ranges))

(define current-check-options (make-parameter '()))

(define* (with-check-options options thunk)
  "Run @var{thunk} with @var{options} -- an alist -- visible to the checkers.

The checker protocol is three arguments, @code{(ast source dialect)}, so
anything configurable -- the line limit, the indentation width, the LOOP
keyword style -- travels beside it rather than through every signature.  A
checker reads what it needs with @code{option}, which always has a default."
  (parameterize ((current-check-options options)) (thunk)))

(define* (option key #:optional (default #f))
  "Return the configured value of @var{key}, or @var{default}."
  (let ((cell (assq key (current-check-options))))
    (if cell (cdr cell) default)))

;;; ===== Checker construction =====

(define* (checker name description dialects proc
                  #:key (severity 'warning) example code)
  "Build a <lint-checker> with its documentation attached.

The record is the whole contract: a name to select it by, a description for
`list-checkers', the dialects it applies to, the severity of what it reports
and, for `explain', a failing example and the fix it expects."
  (make-lint-checker name description dialects proc severity example code))

(define* (make-checker name description dialects proc
                       #:key (severity 'warning) example code)
  (checker name description dialects proc #:severity severity #:example example
           #:code code))

(define* (diagnose checker severity message node #:key fix range)
  "Build a warning about @var{node} on behalf of @var{checker}."
  (make-lint-warning checker severity message (node-start node) fix range))
