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

;;; ===== What an argument count means =====
;;;
;;; A call's arity is decided by the definition it resolves to, and sift has one
;;; already: `parse-definitions' gives every definition in the file with its
;;; scope and its parameter list.  What is read here is the number of arguments
;;; a caller has to pass -- everything before the first `#:optional', `#:key',
;;; `&optional' or rest marker -- which is the number `required-positional-formals'
;;; answers.

(define (formal-marker-text node)
  "Return the marker @var{node} writes without its sigil, or @code{#f}.

`#:optional' and `&optional' are one marker in two dialects, and the sigil is
the only difference: reading what is left of the name is how one question is
asked of both."
  (let ((name (and (node-atom? node) (atom-symbol node))))
    (and name
      (let ((text (symbol->string name)))
        (cond
          [(node-package-marker node) text]
          [(string-prefix? "&" text) (substring text 1)]
          [else #f])))))

(define (formal-arity formals)
  "Return @code{(required . most)} for the parameter list @var{formals}.

`most' counts the optional parameters as well, and is @code{#f} when the list
takes any number of arguments: a rest marker, a dotted tail, or a shape this
rule does not read.  The two numbers are the two things a call can be wrong
about -- one argument too few, one too many -- and a rest list is only wrong
about the first."
  (let loop ((rest formals) (required 0) (optional 0) (optional? #f))
    (cond
      [(null? rest) (cons required (+ required optional))]
      [(not (pair? rest)) (cons required #f)]
      [(not (node-atom? (car rest)))
       (if optional?
         (loop (cdr rest) required (+ optional 1) #t)
         (cons required #f))]
      [else
        (let ((marker (formal-marker-text (car rest))))
          (cond
            [(equal? marker "optional") (loop (cdr rest) required optional #t)]
            [(member marker '("rest" "body")) (cons required #f)]
            [(member marker '("key" "allow-other-keys" "aux"))
             (cons required (+ required optional))]
            [optional? (loop (cdr rest) required (+ optional 1) #t)]
            [else (loop (cdr rest) (+ required 1) optional #f)]))])))

(define (definition-arity definition)
  "Return @code{(required . most)} for @var{definition}, or @code{#f}."
  (let ((formals (definition-formals definition)))
    (and (pair? formals) (formal-arity formals))))

(define (top-level-arities ast)
  "Return a hash from a name to the arity of its top-level definition.

A name defined twice with two different arities is left out rather than
guessed at: one of the two is what a call means, and this rule has no way to
say which."
  (let ((arities (make-hash-table)))
    (for-each
     (lambda (definition)
       (unless (definition-scope definition)
         (let ((arity (definition-arity definition)))
           (when arity
             (let* ((name (definition-name definition))
                    (known (hash-ref arities name 'unknown)))
               (hash-set! arities name
                          (cond
                            [(eq? known 'unknown) arity]
                            [(equal? known arity) arity]
                            [else 'conflicting])))))))
     (parse-definitions ast))
    arities))

(define (lambda-arity node)
  "Return the arity a @code{lambda} bound to a local name takes, or @code{#f}."
  (and (node-list? node)
    (memq (node-head-name node) '(lambda lambda*))
    (let ((formals (car (form-arguments node))))
      (and formals
        (formal-arity (if (node-list? formals) (node-elements formals) formals))))))

(define (local-arity ancestors name)
  "Return the arity a `let' among @var{ancestors} gives @var{name}, or @code{#f}.

A procedure bound in a `let' is the procedure the call in that body means, so
its parameter list is the one worth counting -- and it is a `lambda' the source
states outright, not a guess about a value."
  (let ((wanted (string->symbol name)))
    (any (lambda (ancestor)
           (and (node-list? ancestor)
             (memq (node-head-name ancestor) let-forms)
             (let ((bindings (let-bindings ancestor)))
               (and bindings
                 (any (lambda (binding)
                        (and (node-list? binding)
                          (eq? (atom-symbol (node-head binding)) wanted)
                          (pair? (cdr (node-elements binding)))
                          (lambda-arity (cadr (node-elements binding)))))
                      (node-elements bindings))))))
         ancestors)))

(define (positional-count arguments)
  "Return how many of @var{arguments} a caller passes positionally."
  (let loop ((rest arguments) (count 0))
    (cond
      [(null? rest) count]
      [(keyword-node? (car rest)) count]
      [else (loop (cdr rest) (+ count 1))])))

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

;;; ===== call-arity =====
;;;
;;; The finding a reader wants before running the program: `(make-window ui)'
;;; where `make-window' takes a width.  It is `info' because the definition a
;;; call means can live in another file -- sift reads one file here -- and a
;;; finding that says "in this file" must not read as an error about the
;;; program.

(define (arity-finding name arity supplied node)
  "Return the finding @var{name} earns when called with @var{supplied}, or @code{#f}."
  (let ((required (car arity))
        (most (cdr arity)))
    (cond
      [(< supplied required)
       (make-lint-warning
        'call-arity 'info
        (format #f "`~a` takes ~a argument~a and is called with ~a"
                name required (if (= required 1) "" "s") supplied)
        (node-start (node-head node)))]
      [(and most (> supplied most))
       (make-lint-warning
        'call-arity 'info
        (format #f "`~a` takes at most ~a argument~a and is called with ~a"
                name most (if (= most 1) "" "s") supplied)
        (node-start (node-head node)))]
      [else #f])))

(define (call-arity-warnings ast source dialect)
  (let ((warnings '())
        (arities (top-level-arities ast)))
    (define (note node ancestors)
      (when (and (node-list? node) (call-form? node)
              (not (quoted-context? ancestors))) (let* ((name (called-name node))
                                                        (local (and name (local-arity ancestors name)))
                                                        (arity (and name
                                                                 (not (binding-position? node ancestors))
                                                                 (or local
                                                                   (and (not (shadowed? ancestors name))
                                                                     (hash-ref arities name 'unknown)))))
                                                        (supplied (and arity (positional-count (form-arguments node))))
                                                        (finding (and (pair? arity)
                                                                   (arity-finding name arity supplied node))))
                                                   (when finding (set! warnings (cons finding warnings))))))
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
           "A call that passes fewer arguments than the definition in this file requires"
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
