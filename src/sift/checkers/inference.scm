;; sift --- checkers that read the file as a whole
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Four questions a single form cannot answer, and the line every one of them
;; stops at.  A call's arity is a fact about the definition it resolves to, a
;; literal's type is a fact about the literal, a comparison's right predicate
;; is a fact about the literal it compares against, and an import is unused
;; only when the whole file has been read.  None of them needs a type system:
;; each is decided from what the source says outright, and a value whose type
;; the source does not state is left alone rather than guessed at.
;;
;; That is also why these are separate from the shape rules.  A shape rule can
;; be answered while the form is in front of the reader; these need the
;; definition table, which is the whole file, so they are the ones the language
;; server computes after it has published the findings it can answer at once.

(define-module (sift checkers inference)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift check base)
  #:use-module (sift definitions)
  #:export (inference-checkers
            inference-checker-names
            call-arity-checker
            string-literal-argument-checker
            number-literal-argument-checker
            unused-import-from-checker
            eq-non-symbol-checker))

;;; ===== What an argument list means =====
;;;
;;; A call is wrong about three things, and each of them is counted here:
;;;
;;;   - too few positional arguments, where the definition requires more;
;;;   - too many, where it takes fewer and has no rest parameter;
;;;   - a keyword it does not take, or a keyword left without a value.
;;;
;;; What a definition takes is read from its parameter list, in the markers
;;; both families of dialects write.  `#:optional' and `&optional' add
;;; arguments a caller may pass positionally; `#:key' and `&key' add arguments
;;; a caller passes by keyword, and do not raise the positional ceiling;
;;; `#:rest', `&rest', `&body' and a dotted tail remove the ceiling and, with
;;; it, the set of keywords a caller has to match; `#:allow-other-keys' and
;;; `&allow-other-keys' remove the set alone; `&aux', `&whole' and
;;; `&environment' bind things a caller does not pass at all.
;;;
;;; An arity is `(CLAUSES . KEYWORDS)'.  CLAUSES is a list of `(MIN . MAX)'
;;; pairs, MAX being #f for "as many as the caller has", because a
;;; `case-lambda' is several parameter lists and a count is right when *some*
;;; clause accepts it: `(case-lambda ((a) ...) ((a b c) ...))' takes one
;;; argument or three, and is wrong with two.  KEYWORDS is the list of names
;;; the definition takes as keywords, #f when it takes any, and '() when it
;;; takes none -- which is a different answer from "any", and the difference is
;;; what lets `(area 1 :width)' be read as two arguments rather than one.

(define (marker-name node)
  "Return the name the formal-list marker @var{node} writes, or @code{#f}.

`&optional' in Common Lisp and Emacs Lisp and `#:optional' in Guile are one
marker in two spellings, and `:optional' is the same word written as a
keyword.  Reading what is left of the name is how one question is asked of
all three."
  (let ((value (and (node-atom? node) (node-value node))))
    (and (string? value)
      (let ((marker (node-package-marker node)))
        (cond
          [(and marker (string=? marker "#:"))
           (and (> (string-length value) 2) (substring value 2))]
          [(eq? (node-atom-type node) 'keyword) (substring value 1)]
          [(string-prefix? "&" value) (substring value 1)]
          [else #f])))))

(define (parameters-of node)
  "Return the nodes a parameter list is written as, or @code{'()}.

`(lambda (a b) ...)' keeps them in a list, and `(lambda args ...)' keeps a
single rest parameter where the list would be."
  (cond
    [(node-list? node) (node-elements node)]
    [(and node (node-atom? node)) (list node)]
    [else '()]))

(define (keyword-parameter-name node)
  "Return the name the keyword parameter @var{node} is called by, or @code{#f}.

Guile writes `#:key width', Common Lisp writes `&key width' or
`&key (width 10)', and a destructured key parameter writes the keyword first:
`&key ((:width w) 10)' is called with `:width', whichever variable it binds."
  (cond
    [(node-atom? node) (atom-symbol node)]
    [(node-list? node)
     (let ((head (node-head node)))
       (cond
         [(and head (node-atom? head)) (atom-symbol head)]
         [(and head (node-list? head) (pair? (node-elements head)))
          (atom-symbol (car (node-elements head)))]
         [else #f]))]
    [else #f]))

(define (clauses-of-formals elements)
  "Return @code{(CLAUSES . KEYWORDS)} for one parameter list, or @code{#f}.

@var{elements} is the list of nodes a parameter list is made of.  @code{#f}
is the answer to a parameter list this rule cannot read -- a marker it does
not know, a keyword specification with no name in it -- because a count
guessed from half a list is a finding about sift rather than about the
program, and an omission is cheaper than a wrong answer."
  (let loop ((rest elements) (required 0) (optional 0) (state 'required)
             (keywords '()))
    (cond
      [(null? rest)
       (cons (list (cons required (+ required optional))) keywords)]
      ;; a destructuring parameter, or a `(name default)' pair: one argument,
      ;; whichever part of the parameter list it is written in
      [(not (node-atom? (car rest)))
       (cond
         [(eq? state 'key)
          (let ((name (keyword-parameter-name (car rest))))
            (and name
              (loop (cdr rest) required optional state (cons name keywords))))]
         [else
           (loop (cdr rest)
                 (if (eq? state 'required) (+ required 1) required)
                 (if (eq? state 'optional) (+ optional 1) optional)
                 state keywords)])]
      [else
        (let ((name (marker-name (car rest))))
          (cond
            ;; a dotted tail is a rest parameter spelled with a dot
            [(and (eq? (node-atom-type (car rest)) 'symbol)
               (string=? (node-value (car rest)) "."))
             (cons (list (cons required #f)) #f)]
            [(not name)
             (cond
               [(eq? state 'required)
                (loop (cdr rest) (+ required 1) optional state keywords)]
               [(eq? state 'optional)
                (loop (cdr rest) required (+ optional 1) state keywords)]
               [(eq? state 'key)
                (loop (cdr rest) required optional state
                      (cons (atom-symbol (car rest)) keywords))]
               [else (loop (cdr rest) required optional state keywords)])]
            [(member name '("rest" "body"))
             (cons (list (cons required #f)) #f)]
            [(equal? name "optional")
             (loop (cdr rest) required optional 'optional keywords)]
            [(equal? name "key") (loop (cdr rest) required optional 'key keywords)]
            [(equal? name "aux") (loop (cdr rest) required optional 'aux keywords)]
            [(equal? name "allow-other-keys")
             (loop (cdr rest) required optional state #f)]
            [(member name '("whole" "environment"))
             (loop (cdr rest) required optional state keywords)]
            [else #f]))])))

(define (combine-arities arities)
  "Return the arity that accepts what any of @var{arities} accepts, or @code{#f}.

Two parameter lists are one definition's when a `case-lambda' has clauses, and
they are two answers when a name is defined twice in the file.  Either way the
definition accepts a call that either one accepts: the ranges are united, and
so are the keyword names.  One clause that takes any keyword takes the
question away from the whole, because no other clause's list is the list of
what the definition accepts."
  (let ((real (delete #f arities)))
    (and (pair? real)
      (cons (append-map car real)
            (if (any (lambda (arity) (eq? (cdr arity) #f)) real)
              #f
              (delete-duplicates (append-map cdr real)))))))

(define (lambda-arity-of form)
  "Return the arity a `lambda', `lambda*' or `case-lambda' form states, or #f."
  (let ((head (and (node-list? form) (node-head-name form)))
        (arguments (form-arguments form)))
    (case head
      [(lambda lambda* cl-lambda)
       (and (pair? arguments)
         (clauses-of-formals (parameters-of (car arguments))))]
      [(case-lambda case-lambda*)
       (combine-arities
        (map (lambda (clause)
               (and (node-list? clause)
                 (pair? (node-elements clause))
                 (clauses-of-formals (parameters-of (node-head clause)))))
             arguments))]
      [else #f])))

(define (form-arity form)
  "Return the arity @var{form} states, or @code{#f}.

A definition states one in three shapes: the parameter list beside the name
(`(define (f a) ...)', `(defun f (a) ...)'), a procedure value
(`(define f (lambda (a) ...))'), or a list of clauses
(`(define f (case-lambda (() ...) ((a) ...)))').  `define-syntax' states none:
a macro's arguments are a pattern, and a pattern is not a parameter list."
  (let ((head (and (node-list? form) (node-head-name form)))
        (arguments (form-arguments form)))
    (cond
      [(memq head '(define define* define-public define-inlinable define-macro))
       (let ((first (and (pair? arguments) (car arguments))))
         (if (and first (node-list? first))
           (clauses-of-formals (cdr (node-elements first)))
           (and (pair? (cdr arguments)) (lambda-arity-of (cadr arguments)))))]
      [(memq head '(defun defsubst defmacro cl-defun cl-defsubst cl-defmacro
                          defmethod cl-defmethod defgeneric cl-defgeneric
                          define-minor-mode define-derived-mode define-generic-mode))
       (and (pair? (cdr arguments))
         (clauses-of-formals (parameters-of (cadr arguments))))]
      [(memq head '(defvar defvar-local defcustom defparameter defface defalias))
       (and (pair? (cdr arguments)) (lambda-arity-of (cadr arguments)))]
      [else #f])))

(define (definition-arity definition)
  "Return the arity @var{definition} states, or @code{#f}."
  (and (definition-form definition)
    (form-arity (definition-form definition))))

(define (top-level-arities ast)
  "Return a hash from a name to the arity of its top-level definitions.

A name defined twice is read as the union of both: the two definitions are in
the file, and a call that either one accepts is a call this rule cannot say is
wrong.  The alternative -- saying nothing about the name at all -- threw away
the definitions that do agree, which is most of them."
  (let ((arities (make-hash-table)))
    (for-each
     (lambda (definition)
       (unless (definition-scope definition)
         (let ((arity (definition-arity definition)))
           (when arity
             (hash-set! arities (definition-name definition)
                        (combine-arities
                         (list arity
                               (hash-ref arities
                                         (definition-name definition)
                                         #f))))))))
     (parse-definitions ast))
    arities))

;;; --- what a form binds ------------------------------------------------
;;;
;;; A local binding of the same name makes the file-level definition the wrong
;;; answer, so a call inside a `let' that binds the name is not reported.  The
;;; question is only ever asked about the ancestors of one call, and the answer
;;; has to be cheap: the parameter list of a definition form, and the binding
;;; list of a `let'.

(define let-forms '(let let* letrec letrec* let-values let*-values let-syntax
                     letrec-syntax))

(define (parameter-names node)
  "Return the names the parameter list of @var{node} binds, or @code{'()}."
  (if (formals-holder? node)
    (filter-map atom-symbol (or (formal-elements node) '()))
    '()))

(define (let-bindings node)
  "Return the binding list of a @code{let} form, or @code{#f}."
  (let ((arguments (form-arguments node)))
    (and (pair? arguments)
      (let* ((first (car arguments))
             (bindings (if (and (node-atom? first) (pair? (cdr arguments)))
                         (cadr arguments)
                         first)))
        (and (node-list? bindings) bindings)))))

(define (binding-list-names node)
  "Return the names a binding list binds, in the shape `let' writes them."
  (if (not (list-headed-by? node 'let-values))
    (filter-map (lambda (binding)
                  (and (node-list? binding) (atom-symbol (node-head binding))))
                (node-elements node))
    (append-map (lambda (binding)
                  (and (node-list? binding)
                    (filter-map atom-symbol (node-elements (node-head binding)))))
                (node-elements node))))

(define (bound-names node)
  "Return the names @var{node} binds, in every shape a binding takes."
  (define (names-of thunk) (or (thunk) '()))
  (append
   (parameter-names node)
   (names-of (lambda ()
               (and (memq (node-head-name node) let-forms)
                 (let ((bindings (let-bindings node)))
                   (and bindings (binding-list-names bindings))))))
   (names-of (lambda ()
               (and (list-headed-by? node 'do)
                 (let ((variables (car (form-arguments node))))
                   (and (node-list? variables)
                     (filter-map (lambda (variable)
                                   (and (node-list? variable)
                                     (atom-symbol (node-head variable))))
                                 (node-elements variables)))))))
   (names-of (lambda ()
               (and (memq (node-head-name node) '(dolist dotimes))
                 (let ((arguments (form-arguments node)))
                   (and (pair? arguments)
                     (let ((spec (car arguments)))
                       (and (node-list? spec)
                         (list (atom-symbol (node-head spec))))))))))))

(define (shadowed? ancestors name)
  "Return @code{#t} when one of @var{ancestors} binds @var{name}.

@var{name} comes from the call site as the text of an atom, and what a
binding list yields is a symbol: the comparison is made in the one currency
both can be written in."
  (let ((wanted (string->symbol name)))
    (any (lambda (ancestor)
           (and (node-list? ancestor)
             (member wanted (bound-names ancestor))))
         ancestors)))

(define (binding-position? node ancestors)
  "Return @code{#t} when @var{node} is a binding of a `let`, not a call.

`(let ((area (lambda (x) x))) ...)' holds a list headed by `area', and a
walker that took every list headed by a known name for a call would report
the binding as one.  The binding list is recognised by its position: it is
the child of a `let' that holds the bindings."
  (and (pair? ancestors) (pair? (cdr ancestors))
    (let ((list (car ancestors))
          (form (cadr ancestors)))
      (and (node-list? form)
        (memq (node-head-name form) let-forms)
        (let ((bindings (let-bindings form)))
          (and bindings (eq? bindings list)
            (any (lambda (binding) (eq? binding node))
                 (node-elements list))))))))

(define (called-name node)
  "Return the symbol @var{node} calls, or @code{#f}."
  (let ((head (node-head node)))
    (and head (node-atom? head) (eq? (node-atom-type head) 'symbol)
      (node-value head))))

;;; --- which definition a call means -------------------------------------
;;;
;;; The file's own definitions are one answer, and the forms around the call
;;; are the others.  A procedure a `let' binds, a loop a `let' names, a
;;; definition written inside the body the call sits in: each of them is the
;;; procedure the call means, and each is nearer than the file.

(define (lambda-arity node)
  "Return the arity a @code{lambda} bound to a local name takes, or @code{#f}."
  (and (node-list? node) (lambda-arity-of node)))

(define (named-let-arity ancestor name)
  "Return the arity a named `let' gives @var{name}, or @code{#f}.

`(let loop ((i 0) (j 1)) ...)' binds a procedure whose parameters are the
loop's variables, so `(loop 0 1)' is the call that will not raise.  The
binding list is the parameter list; nothing is guessed."
  (let ((arguments (form-arguments ancestor)))
    (and (pair? arguments)
      (pair? (cdr arguments))
      (node-atom? (car arguments))
      (eq? (atom-symbol (car arguments)) (string->symbol name))
      (node-list? (cadr arguments))
      (let ((count (length (node-elements (cadr arguments)))))
        (cons (list (cons count count)) '())))))

(define (local-arity ancestors name)
  "Return the arity a binding among @var{ancestors} gives @var{name}, or #f.

A procedure bound in a `let' is the procedure the call in that body means, so
its parameter list is the one worth counting -- and it is a `lambda' the
source states outright, not a guess about a value.  A named `let' is the other
shape a local procedure takes."
  (let ((wanted (string->symbol name)))
    (any (lambda (ancestor)
           (and (node-list? ancestor)
             (memq (node-head-name ancestor) let-forms)
             (or (named-let-arity ancestor name)
               (let ((bindings (let-bindings ancestor)))
                 (and bindings
                   (any (lambda (binding)
                          (and (node-list? binding)
                            (eq? (atom-symbol (node-head binding)) wanted)
                            (pair? (cdr (node-elements binding)))
                            (lambda-arity (cadr (node-elements binding)))))
                        (node-elements bindings)))))))
         ancestors)))

(define (node-index-of node)
  "Return the character offset @var{node} starts at."
  (source-location-index (node-start node)))

(define (defined-name-of form)
  "Return the name a `define' form binds, or @code{#f}.

`(define (f a) ...)' keeps the name in the head of its second element and
`(define f (lambda ...))' keeps it whole in the second; reading the second
element answers both, which is what makes an internal definition comparable
with the name a call writes."
  (let ((elements (node-elements form)))
    (and (pair? elements)
      (pair? (cdr elements))
      (let ((second (cadr elements)))
        (cond
          [(and second (node-list? second))
           (let ((head (node-head second)))
             (and head (node-atom? head) (atom-symbol head)))]
          [(and second (node-atom? second)) (atom-symbol second)]
          [else #f])))))

(define (internal-definition? form name)
  "Return @code{#t} when @var{form} defines a procedure called @var{name}."
  (and (node-list? form)
    (memq (node-head-name form)
          '(define define* define-public define-inlinable define-macro))
    (eq? (defined-name-of form) (string->symbol name))))

(define (internal-arity ancestors name index)
  "Return the arity of a definition inside a body @var{index} is in, or #code{#f}.

`(define (outer) (define (inner a b) 0) (inner 1))' is the case: `inner' is a
definition the file states outright, and the call means it.  The nearest body
wins, and inside it the last definition before the call, because a body that
defines a name twice is a body whose later definition the later calls mean."
  (any
   (lambda (ancestor)
     (cond
       [(internal-definition? ancestor name) (form-arity ancestor)]
       [else
         (let* ((definitions
                 (if (node-list? ancestor)
                   (filter (lambda (form) (internal-definition? form name))
                           (node-elements ancestor))
                   '()))
                (before (filter (lambda (definition)
                                  (< (node-index-of definition) index))
                                definitions))
                (chosen (if (pair? before)
                          (last before)
                          (and (pair? definitions) (car definitions)))))
           (and chosen (form-arity chosen)))]))
   ancestors))

;;; ===== call-arity =====
;;;
;;; The finding a reader wants before running the program: `(make-window ui)'
;;; where `make-window' takes a width.  It is `info' because the definition a
;;; call means can live in another file -- sift reads one file here -- and a
;;; finding that says "in this file" must not read as an error about the
;;; program.

(define (keyword-name node)
  "Return the name the keyword @var{node} is written with, or @code{#f}."
  (and (keyword-node? node) (atom-symbol node)))

(define (keyword-sigil node)
  "Return the spelling `#:' or `:' that @var{node} writes a keyword with.

A message that lists the keywords a definition takes should spell them the way
the call in front of the reader spells them, and the spelling is visible in
the keyword that was written."
  (let ((value (and (node-atom? node) (node-value node))))
    (if (and (string? value) (string-prefix? "#:" value)) "#:" ":")))

(define (call-shape arguments arity)
  "Return @code{(positionals . problems)} for @var{arguments} read as a call.

A definition that takes keywords ends its positional arguments at the first
keyword -- `(make-window root #:height 3)' passes one argument, not three --
and a definition that takes none has no such place to stop, so `(area 1 :x)'
passes two.  A definition whose keywords are not known takes keywords without
being checked for them.

@var{problems} is a list of @code{(REASON NAME NODE)}: `unknown-keyword' for a
keyword the definition does not take, `missing-value' for one left at the end
of the call.  Each carries the node it is written at, because a finding about
a keyword should point at the keyword."
  (let* ((clauses (car arity))
         (keywords (cdr arity))
         (keyed? (not (null? keywords)))
         (floor (if (null? clauses) 0 (apply min (map car clauses)))))
    (let loop ((rest arguments) (positionals 0) (in-keywords? #f)
               (problems '()))
      (cond
        [(null? rest) (cons positionals (reverse problems))]
        [(and (keyword-node? (car rest))
           (or in-keywords? (and keyed? (>= positionals floor))))
         (let* ((keyword (car rest))
                (name (keyword-name keyword))
                (value (and (pair? (cdr rest)) (cadr rest)))
                (unknown (and (list? keywords) (not (memq name keywords)))))
           (if value
             (loop (cddr rest) positionals #t
                   (if unknown
                     (cons (list 'unknown-keyword name keyword) problems)
                     problems))
             (cons positionals
                   (reverse
                    (cons (list 'missing-value name keyword)
                          (if unknown
                            (cons (list 'unknown-keyword name keyword) problems)
                            problems))))))]
        [else
          (loop (cdr rest) (+ positionals 1) in-keywords? problems)]))))

(define (clause-count-phrase clause)
  "Return the counts @var{clause} accepts, without the word `argument'."
  (let ((least (car clause))
        (most (cdr clause)))
    (cond
      [(not most) (format #f "at least ~a" least)]
      [(= least most) (format #f "~a" least)]
      [else (format #f "~a to ~a" least most)])))

(define (argument-noun clause)
  "Return the word a sentence names the counts of @var{clause} with."
  (let ((least (car clause))
        (most (cdr clause)))
    (if (and (or (not most) (= least most)) (= least 1))
      "argument"
      "arguments")))

(define (merge-clauses clauses)
  "Return @var{clauses} sorted, with the ranges that touch written as one.

`(1 . 2)' and `(3 . 4)' are one range to a reader looking for a count that is
accepted, and printing them apart would make a message that names the same
argument twice."
  (let loop ((rest (sort (delete-duplicates clauses)
                         (lambda (a b) (< (car a) (car b)))))
             (merged '()))
    (cond
      [(null? rest) (reverse merged)]
      [(null? merged) (loop (cdr rest) (list (car rest)))]
      [else
        (let ((current (car merged))
              (next (car rest)))
          (if (and (cdr current) (<= (car next) (+ (cdr current) 1)))
            (loop (cdr rest)
                  (cons (cons (car current)
                              (if (cdr next) (max (cdr current) (cdr next)) #f))
                        (cdr merged)))
            (loop (cdr rest) (cons next merged))))])))

(define (arity-phrase arity)
  "Return the words a message names what @var{arity} accepts with."
  (let* ((clauses (merge-clauses (car arity)))
         (last-clause (and (pair? clauses) (last clauses))))
    (if (null? clauses)
      "any number of arguments"
      (format #f "~a ~a"
              (string-join (map clause-count-phrase clauses) " or ")
              (argument-noun last-clause)))))

(define (clause-accepts? clause supplied)
  "Return @code{#t} when @var{clause} accepts @var{supplied} arguments."
  (and (>= supplied (car clause))
    (or (not (cdr clause)) (<= supplied (cdr clause)))))

(define (count-finding name arity supplied node)
  "Return the finding a call passing @var{supplied} positionals earns, or #f."
  (and (not (any (lambda (clause) (clause-accepts? clause supplied))
                 (car arity)))
    (make-lint-warning
     'call-arity 'info
     (format #f "`~a` takes ~a and is called with ~a"
             name (arity-phrase arity) supplied)
     (node-start (node-head node)))))

(define (keyword-finding name arity problem)
  "Return the finding one keyword @var{problem} earns."
  (let ((reason (car problem))
        (node (caddr problem)))
    (make-lint-warning
     'call-arity 'info
     (case reason
       [(unknown-keyword)
        (let ((taken (cdr arity)))
          (if (null? taken)
            (format #f "`~a` takes no keyword arguments, and is called with `~a`"
                    name (node-value node))
            (format #f "`~a` does not take `~a`; it takes ~a"
                    name (node-value node)
                    (string-join
                     (map (lambda (taken-keyword)
                            (format #f "`~a~a`"
                                    (keyword-sigil node) taken-keyword))
                          taken)
                     ", "))))]
       [(missing-value)
        (format #f "`~a` is called with `~a` and no value"
                name (node-value node))]
       [else (format #f "`~a` is called with a keyword it cannot take" name)])
     (node-start node))))

(define (call-arity-of name node ancestors arities)
  "Return the arity the call @var{node} resolves to, or @code{#f}.

Nearest first: a procedure a `let' binds, a loop a `let' names, a definition
inside the body the call is written in, then the file's own.  A name the
enclosing forms bind as something other than a procedure makes the file's
definition the wrong answer, and no answer is better than a wrong one."
  (or (local-arity ancestors name)
    (internal-arity ancestors name (node-index-of node))
    (and (not (shadowed? ancestors name))
      (hash-ref arities name #f))))

(define (call-arity-warnings ast source dialect)
  (let ((warnings '())
        (arities (top-level-arities ast)))
    (define (note node ancestors)
      (when (and (node-list? node) (call-form? node)
              (not (quoted-context? ancestors))
              (not (in-formals? node ancestors)))
        (let* ((name (called-name node))
               (arity (and name
                        (not (binding-position? node ancestors))
                        (call-arity-of name node ancestors arities)))
               (shape (and arity (call-shape (form-arguments node) arity))))
          (when shape
            (for-each
             (lambda (finding)
               (when finding (set! warnings (cons finding warnings))))
             (append (list (count-finding name arity (car shape) node))
                     (map (lambda (problem)
                            (keyword-finding name arity problem))
                          (cdr shape))))))))
    (walk-with-ancestors note ast)
    (reverse warnings)))

(define string-argument-positions
  '((string-append . all)
    (string-length 0)
    (string-upcase 0)
    (string-downcase 0)
    (string-trim 0)
    (string-trim-both 0)
    (string-trim-right 0)
    (string-null? 0)
    (string-reverse 0)
    (string-copy 0)
    (string-split 0)
    (string-contains 0 1)
    (string-prefix? 0 1)
    (string-suffix? 0 1)
    (string=? 0 1)
    (string<? 0 1)
    (string>? 0 1)
    (string<=? 0 1)
    (string>=? 0 1)
    (string-ci=? 0 1)
    (string-pad 0)
    (string-pad-right 0)))

;; The operators whose arguments are numbers, and only numbers.
(define number-argument-operators
  '(= < > <= >= + - * / quotient remainder modulo expt abs sqrt min max
      1+ 1- zero? positive? negative? even? odd?))

(define (literal-kind node)
  "Return what kind of literal @var{node} is, or @code{#f}."
  (and (node-atom? node)
    (case [node-atom-type node]
      [(number) 'number]
      [(string) 'string]
      [(character) 'character]
      [(boolean) 'boolean]
      [else #f])))

(define (string-position? positions index)
  "Return @code{#t} when argument @var{index} is one of @var{positions}.

A single position is written as the integer it is -- `(string-length 0)' is
an improper pair with @code{0} in its tail -- and `all' is every argument."
  (cond
    [(eq? positions 'all) #t]
    [(list? positions) (and (memv index positions) #t)]
    [else (= index positions)]))

(define (string-literal-argument-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (let* ((name (node-head-name node))
              (positions (and name (assq-ref string-argument-positions name))))
         (when positions
           (let loop ((arguments (form-arguments node)) (index 0))
             (unless (null? arguments)
               (let ((kind (literal-kind (car arguments))))
                 (when (and (string-position? positions index)
                         kind (not (eq? kind 'string)))
                   (set! warnings
                         (cons (make-lint-warning
                                'string-literal-argument 'warning
                                (format #f "`~a` takes a string here, and ~a is a ~a"
                                        name (node-source source (car arguments)) kind)
                                (node-start (car arguments)))
                               warnings))))
               (loop (cdr arguments) (+ index 1)))))))
     ast)
    (reverse warnings)))

(define (number-literal-argument-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (let ((name (node-head-name node)))
         (when (and name (memq name number-argument-operators))
           (for-each
            (lambda (argument)
              (when (eq? (literal-kind argument) 'string)
                (set! warnings
                      (cons (make-lint-warning
                             'number-literal-argument 'warning
                             (format #f "`~a` works on numbers, and ~a is a string"
                                     name (node-source source argument))
                             (node-start argument))
                            warnings))))
            (form-arguments node)))))
     ast)
    (reverse warnings)))

;;; ===== eq-non-symbol =====
;;;
;;; `eq' is identity: two separately read numbers need not be the same object,
;;; and two equal strings almost never are.  A comparison with a literal is
;;; therefore decidable -- there is nothing to infer -- and the predicate that
;;; decides it is the one the literal's type asks for.

(define comparison-forms
  '((eq . (symbol eql equal))
    (eq? . (symbol eqv? equal?))
    (memq . (symbol member member))
    (assq . (symbol assoc assoc))))

(define (comparison-parts node)
  "Return @code{(predicate . arguments)} for a comparison form, or @code{#f}."
  (let ((name (node-head-name node)))
    (and name (assq-ref comparison-forms name))))

(define (eq-non-symbol-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (let* ((name (node-head-name node))
              (predicates (comparison-parts node)))
         (when predicates
           (let loop ((arguments (form-arguments node)))
             (unless (null? arguments)
               (let* ((argument (car arguments))
                      (kind (literal-kind argument))
                      (wanted (and kind
                                (case kind
                                  [(string) (caddr predicates)]
                                  [(number character) (cadr predicates)]
                                  [else #f]))))
                 (when wanted
                   (set! warnings
                         (cons (make-lint-warning/range
                                'eq-non-symbol 'warning
                                (format #f "`~a` compares identity, and ~a is a ~a; `~a` compares it"
                                        name (node-source source argument) kind wanted)
                                (node-start (node-head node))
                                (node-index (node-head node))
                                (node-end-index (node-head node))
                                (lambda (text start end) (symbol->string wanted)))
                               warnings))))
               (loop (cdr arguments)))))))
     ast)
    (reverse warnings)))

;;; ===== unused-import-from =====
;;;
;;; A Common Lisp package definition imports a name to use it; one that is never
;;; used is a name the reader has to look up for nothing, and `:import-from'
;;; makes that silence invisible -- the symbol is in the package either way.

(define (defpackage-form? node)
  (list-headed-by? node 'defpackage))

(define (import-from-clause? node)
  (and (node-list? node)
    (eq? (atom-symbol (node-head node)) 'import-from)))

(define (imported-names clause)
  "Return @code{(package . names)} for an @code{:import-from} clause."
  (let ((arguments (form-arguments clause)))
    (and (pair? arguments)
      (cons (atom-symbol (car arguments))
            (filter-map atom-symbol (cdr arguments))))))

(define (used-names ast)
  "Return a hash of every symbol name the file uses outside a package definition."
  (let ((used (make-hash-table)))
    (walk-with-ancestors
     (lambda (node ancestors)
       (when (and (node-atom? node)
               (memq (node-atom-type node) '(symbol keyword))
               (not (any defpackage-form? ancestors)))
         (let ((name (atom-symbol node)))
           (when name (hash-set! used name #t)))))
     ast)
    used))

(define (unused-import-from-warnings ast source dialect)
  (let ((warnings '())
        (used (used-names ast)))
    (for-each-form
     (lambda (node)
       (when (defpackage-form? node)
         (for-each
          (lambda (clause)
            (when (import-from-clause? clause)
              (let* ((parts (imported-names clause))
                     (package (car parts))
                     (names (cdr parts)))
                (for-each
                 (lambda (name)
                   (unless (hash-ref used name #f)
                     (for-each
                      (lambda (candidate)
                        (when (and (node-atom? candidate)
                                (eq? (atom-symbol candidate) name))
                          (set! warnings
                                (cons (make-lint-warning
                                       'unused-import-from 'info
                                       (format #f "`~a` is imported from ~a and never used"
                                               name package)
                                       (node-start candidate))
                                      warnings))))
                      (node-elements clause))))
                 names))))
          (form-arguments node))))
     ast)
    (reverse warnings)))

;;; ===== The checkers =====

(define inference-checker-names
  '(call-arity string-literal-argument number-literal-argument
               eq-non-symbol unused-import-from))

(define call-arity-checker
  (checker 'call-arity
           "A call whose arguments do not match the definition in this file: too few, too many, or a keyword it does not take"
           '(scheme elisp cl)
           call-arity-warnings
           #:severity 'info
           #:example '((input . "(define (area width height) 0)\n(area 2)\n")
                       (fix . "(area 2 3)"))))

(define string-literal-argument-checker
  (checker 'string-literal-argument
           "A number or boolean passed where the operator's convention says a string"
           '(scheme elisp cl)
           string-literal-argument-warnings
           #:severity 'warning
           #:example '((input . "(string-append \"a\" 1)")
                       (fix . "(string-append \"a\" \"1\")"))))

(define number-literal-argument-checker
  (checker 'number-literal-argument
           "A string passed to an operator whose arguments are numbers"
           '(scheme elisp cl)
           number-literal-argument-warnings
           #:severity 'warning
           #:example '((input . "(+ 1 \"2\")")
                       (fix . "(+ 1 2)"))))

(define eq-non-symbol-checker
  (checker 'eq-non-symbol
           "`eq`/`eq?` of a literal that identity cannot decide, where the value predicate can"
           '(scheme elisp cl)
           eq-non-symbol-warnings
           #:severity 'warning
           #:example '((input . "(eq? x 1)")
                       (fix . "(eqv? x 1)"))))

(define unused-import-from-checker
  (checker 'unused-import-from
           "A name a `defpackage` imports with `:import-from` and the file never uses"
           '(cl)
           unused-import-from-warnings
           #:severity 'info
           #:example '((input . "(defpackage :demo (:use :cl) (:import-from :alexandria #:curry))\n(in-package :demo)\n")
                       (fix . "(defpackage :demo (:use :cl))\n(in-package :demo)\n"))))

(define inference-checkers
  (list call-arity-checker
        string-literal-argument-checker
        number-literal-argument-checker
        eq-non-symbol-checker
        unused-import-from-checker))
