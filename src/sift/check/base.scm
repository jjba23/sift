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
            walk-forms
            for-each-top-level
            fold-nodes
            node-source
            quoted-context?
            in-formals?
            in-vector?
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
            line-and-column
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

A *form* is a node in a position where code is evaluated.  Four places are
not: anything inside a quote, the datum of a `case` clause, a parameter list
and a vector literal.  A rule about the shape of a form asks this walker and
never has to ask whether what it is looking at is code at all -- which is what
stops `(if . 2)' in an alist from being reported as an `if` with no else
branch, and `#(if a b)' from being reported as one either."
  (walk-forms (lambda (node ancestors) (proc node)) ast #:ancestors? #f))

(define (fold-nodes kons knil ast)
  "Fold @var{kons} over every datum in @var{ast}."
  (let ((acc knil))
    (for-each-node (lambda (node) (set! acc (kons node acc))) ast)
    acc))

(define (walk-with-ancestors proc ast)
  "Call @var{proc} with each datum and the list of forms that enclose it.

The ancestors come nearest-first, which is the order a checker asks them in:
\"is there a `let' between me and that `lambda'?\"

Unlike @code{for-each-form} this visits every node, comments and whitespace
included, because a checker that asks this question is asking about
positions rather than about shapes."
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

;;; ===== Where code is evaluated =====
;;;
;;; The four places a node is data rather than code are decided by looking at
;;; the node and at the forms above it.  Asking the question by walking back up
;;; the ancestors for every node is what made a lint cost more than a format by
;;; two orders of magnitude: the walk up happens once per node per level, and
;;; each step of it asks what a form is called.
;;;
;;; So the same question is answered twice: `walk-forms' carries the answer
;;; down with it, and `code-position?' recomputes it from an ancestor list for
;;; the callers and the tests that only have one.  The two must agree, and the
;;; suite walks a corpus of everything that is ambiguous -- quotes, quasiquotes,
;;; vectors, `case` clauses, parameter lists -- through both and compares.

(define formals-form-heads
  '(lambda define define* define-public defun defmacro defsubst
     cl-defun cl-defmacro defmethod defgeneric))

(define quote-opening-tokens '("'" "`" "#`"))
(define quote-closing-tokens '("," ",@" "#," "#,@"))
(define quote-opening-heads '(quote quasiquote quasisyntax))
(define quote-closing-heads
  '(unquote unquote-splicing unsyntax unsyntax-splicing))

(define (formals-of-form node)
  "Return the parameter list of @var{node} when it is a form that has one."
  (and (node-list? node)
    (memq (node-head-name node) formals-form-heads)
    (let ((elements (node-elements node)))
      (and (pair? elements) (pair? (cdr elements)) (cadr elements)))))

(define* (walk-forms proc ast #:key (ancestors? #t))
  "Call @var{proc} on every node in a position where code is evaluated.

@var{proc} receives the node and, when @var{ancestors?} is true, the forms
that enclose it, nearest first.

Whether a node is code is decided while descending -- from three flags that
say whether the walk is inside a quotation, a vector or a parameter list,
and from the two nearest forms, whose names are carried down with them --
so a whole tree is walked once rather than once per level of nesting.

The `case` test needs the head of the node's parent and the name of its
grandparent, so those are carried beside the flags: @var{parent-head} is the
first datum of the node's parent, @var{parent-head-name} the name of that
parent, and @var{gp-is-case?} says whether the grandparent is one of the forms
whose clauses begin with a datum."
  (define (walk node quoted? in-vector? in-formals?
                gp-is-case? parent-head parent-head-name ancestors)
    (unless (node-commented-out? node)
      (when (and (not (node-trivia? node))
              (not (node-vector? node))
              (not (node-bytevector? node))
              (not quoted?)
              (not in-vector?)
              (not in-formals?)
              (not (and gp-is-case? (eq? parent-head node))))
        (proc node (if ancestors? ancestors '())))
      (cond
        [(node-list? node)
         (let* ((head (node-head-name node))
                (quoted-next (cond [(memq head quote-opening-heads) #t]
                                   [(memq head quote-closing-heads) #f]
                                   [else quoted?]))
                (vector-next (if (memq head quote-closing-heads) #f in-vector?))
                (data-children? (or (node-vector? node) (node-bytevector? node)))
                (container? (memq head formals-form-heads))
                (formals (and container? (formals-of-form node)))
                ;; what the children of this node see: their parent is this
                ;; node, whose own name and parent are exactly what the `case`
                ;; test of a child needs
                (child-gp-is-case? (and parent-head-name
                                     (memq parent-head-name case-form-heads)
                                     #t))
                (child-parent-head (node-head node))
                (child-parent-head-name head)
                (child-ancestors (and ancestors? (cons node ancestors))))
           (for-each
            (lambda (child)
              (walk child
                    quoted-next
                    ;; a vector literal is data, and so is everything in it
                    (if data-children? #t vector-next)
                    (if container?
                      (and formals
                        (<= (node-index formals) (node-index child))
                        (<= (node-end-index child) (node-end-index formals)))
                      in-formals?)
                    child-gp-is-case? child-parent-head child-parent-head-name
                    child-ancestors))
            (node-children node)))]
        [(node-prefix? node)
         (let ((token (node-token node))
               (child (node-child node)))
           (when (and child (not (node-datum-comment? node)))
             (walk child
                   (cond [(member token quote-opening-tokens) #t]
                         [(member token quote-closing-tokens) #f]
                         [else quoted?])
                   (if (member token quote-closing-tokens) #f in-vector?)
                   in-formals?
                   ;; a prefix is a parent like any other, and not a list, so
                   ;; nothing below it can be the datum of a `case` clause
                   #f #f #f
                   (if ancestors? (cons node ancestors) '()))))]
        ;; a conditional is not an ancestor: `#+foo (if a b)' is an `if' in
        ;; the same place the reader found it
        [(node-conditional? node)
         (let ((feature (node-feature node))
               (guarded (node-guarded-form node)))
           (when feature
             (walk feature quoted? in-vector? in-formals?
                   gp-is-case? parent-head parent-head-name ancestors))
           (when guarded
             (walk guarded quoted? in-vector? in-formals?
                   gp-is-case? parent-head parent-head-name ancestors)))]
        [else #f])))
  (for-each (lambda (node)
              (when (node-data? node)
                (walk node #f #f #f #f #f #f '())))
            ast))

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

Walking up from a node, the first ancestor that is a quote, a quasiquote, a
quasisyntax or a `quote`-style form decides: inside it the node is data, and a
list that happens to start with `cond` is not a `cond` form.

An unquote ends the data, whichever of its spellings it is written in --
`,`/`(unquote ...)`, `,@`/`(unquote-splicing ...)`, and the syntax-case
`#,' and `#,@' -- because what is inside it is evaluated again."
  (let loop ((rest ancestors))
    (cond
      [(null? rest) #f]
      [(and (node-prefix? (car rest))
         (member (node-token (car rest)) '("'" "`" "#`")))
       #t]
      [(and (node-list? (car rest))
         (memq (node-head-name (car rest)) '(quote quasiquote quasisyntax)))
       #t]
      [(and (node-prefix? (car rest))
         (member (node-token (car rest)) '("," ",@" "#," "#,@")))
       #f]
      [(and (node-list? (car rest))
         (memq (node-head-name (car rest))
               '(unquote unquote-splicing unsyntax unsyntax-splicing)))
       #f]
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

(define (in-vector? ancestors)
  "Return @code{#t} when @var{node} sits inside a vector literal.

`#(if a b)' is a vector that holds three symbols, not an `if' with no else
branch: a vector literal is data, so a rule about the shape of a form has no
business reading what is inside one.  An unquote -- `,', `,@' or one of their
syntax-case spellings -- ends the data again, exactly as it does in
@code{quoted-context?}: what is inside it is evaluated."
  (let loop ((rest ancestors))
    (cond
      [(null? rest) #f]
      [(or (node-vector? (car rest)) (node-bytevector? (car rest))) #t]
      [(and (node-prefix? (car rest))
         (member (node-token (car rest)) '("," ",@" "#," "#,@")))
       #f]
      [(and (node-list? (car rest))
         (memq (node-head-name (car rest))
               '(unquote unquote-splicing unsyntax unsyntax-splicing)))
       #f]
      [else (loop (cdr rest))])))

(define (code-position? node ancestors)
  "Return @code{#t} when a rule may read @var{node} as a form.

Four places are data rather than code: anything inside a quote, the datum of
a `case` clause, a parameter list, and a vector literal.  A vector literal is
not a form itself, and nothing inside one is, so `#(if a b)' is left alone.
Every rule that asks about the shape of a form asks this first."
  (not (or (node-vector? node)
         (node-bytevector? node)
         (quoted-context? ancestors)
         (case-datum? node ancestors)
         (in-formals? node ancestors)
         (in-vector? ancestors))))

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
;;;
;;; Where the lines of a text begin depends on the text and on nothing else,
;;; and it is asked once per finding -- and, by a checker that reports a
;;; position for every match it finds, once per match.  Working it out again
;;; for every question is what made a walk over a two-thousand-line file cost
;;; ten seconds, so it is worked out once per text and kept until a different
;;; text is asked about.  One entry is enough because a run reads one source at
;;; a time; a different string is a cache miss, never a wrong answer.

(define *lines* (vector #f #f))

(define (compute-line-starts source)
  "Return a vector of the character index at which each line of @var{source} begins."
  (let* ((n (string-length source))
         (starts (let loop ((i 0) (acc (list 0)))
                   (cond
                     [(>= i n) (reverse! acc)]
                     [(char=? (string-ref source i) #\newline)
                      (loop (+ i 1) (cons (+ i 1) acc))]
                     [else (loop (+ i 1) acc)]))))
    (list->vector starts)))

(define (compute-line-spans source starts)
  "Return @code{(start . end)} character offsets for every line of @var{source}.

@var{end} excludes the line break, so a slice of the source is exactly the
line the user sees.  A file that ends with a newline has a final empty line,
which is what a text editor shows too."
  (let* ((n (string-length source))
         (count (vector-length starts)))
    (let loop ((i 0) (out '()))
      (if (>= i count)
        (reverse! out)
        (loop (+ i 1)
              (cons (cons (vector-ref starts i)
                          (if (< (+ i 1) count)
                            (- (vector-ref starts (+ i 1)) 1)
                            n))
                    out))))))

(define (line-index source)
  "Return @code{#(starts spans)} for @var{source}, computed once per text.

The cache is keyed by @code{eq?} on the string, so a text that is a different
object is read again rather than assumed to be the same one: the answer is
never wrong, at worst it is worked out twice."
  (if (eq? (vector-ref *lines* 0) source)
    (vector-ref *lines* 1)
    (let* ((starts (compute-line-starts source))
           (entry (vector starts (compute-line-spans source starts))))
      (vector-set! *lines* 0 source)
      (vector-set! *lines* 1 entry)
      entry)))

(define (line-spans source)
  "Return @code{(start . end)} character offsets for every line of @var{source}."
  (vector-ref (line-index source) 1))

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

(define (line-number-of index starts)
  "Return the 1-based line that holds @var{index}, given where the lines @var{starts}.

A binary search, because a file of two thousand lines is asked this question
once per finding, and walking the lines to find one is the difference between
a lint that finishes and a lint that does not."
  (let loop ((low 0) (high (- (vector-length starts) 1)) (answer 0))
    (if (> low high)
      (+ answer 1)
      (let ((middle (quotient (+ low high) 2)))
        (if (<= (vector-ref starts middle) index)
          (loop (+ middle 1) high middle)
          (loop low (- middle 1) answer))))))

(define (line-and-column source index)
  "Return @code{(values line column)} of character @var{index} in @var{source}.

Both count from one.  An index past the end of the text is answered the way
the line list always answered it: the line after the last one, at its first
column."
  (let* ((entry (line-index source))
         (starts (vector-ref entry 0))
         (number (line-number-of index starts)))
    (if (<= index (string-length source))
      (values number
              (+ 1 (- index (vector-ref starts (- number 1)))))
      (values (+ (vector-length starts) 1) 1))))

(define (line-of-index source index)
  "Return the 1-based line of character @var{index} in @var{source}."
  (call-with-values (lambda () (line-and-column source index))
    (lambda (line column) line)))

(define (location-at-index source index)
  "Build a location from an index alone.

A checker is handed the AST and the text, not the file name, so the file is
left @code{#f} here and the driver fills it in from the run's context.  The
line and column are counted in characters, exactly as the reader counts
them."
  (call-with-values (lambda () (line-and-column source index))
    (lambda (line column)
      (make-source-location #f line column index index))))

(define* (blank-line? text #:optional (start 0) (end (string-length text)))
  "Return @code{#t} when @var{text} holds nothing but whitespace.

@var{start} and @var{end} narrow the question to one line of a larger text,
which is what a checker that has the offsets already wants: asking it of a
substring would be a copy of the line to find out that it is blank."
  (let loop ((i start))
    (cond
      [(>= i end) #t]
      [(memv (string-ref text i) '(#\space #\tab #\return)) (loop (+ i 1))]
      [else #f])))

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
multi-line block comment is not indentation.  One pass, because the walk is
over the whole tree and doing it twice doubles the cost of every checker that
asks."
  (reverse
   (fold-nodes (lambda (node acc)
                 (if (or (node-block-comment? node) (node-string? node))
                   (cons (cons (node-index node) (node-end-index node)) acc)
                   acc))
               '() ast)))

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
