;; sift --- definitions, bindings and references
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Go to definition is the one question an editor asks that sift's reader
;; already knows most of the answer to: a definition is a form whose operator
;; is in the table below, and its name is the atom the form puts in a
;; particular place.  What the reader does not have, and this module adds, is
;; the two things that make the answer worth trusting.
;;
;; The first is scope.  `(define (f x) (let ((x 1)) x))' has two `x's, and the
;; reference belongs to the nearest one; a server that answered with the first
;; `x' in the file would be wrong in every file that uses a name twice.  So the
;; bindings a form introduces are computed here -- parameters, `let' and
;; `letrec' bindings, an internal definition, a named `let', a `dolist'
;; variable -- next to the range each of them is visible in, and a reference is
;; resolved by walking outwards from the form it sits in.
;;
;; The second is what counts as a definition in each dialect.  `defun',
;; `cl-defun', `define', `define-public', `defmacro', `cl-defgeneric',
;; `defstruct', `defpackage', `defalias' and the rest of the table below are
;; not one shape: `(define (f x) ...)' keeps its name inside the argument list,
;; `(defpackage #:foo ...)' keeps it in a keyword, `(defalias 'f ...)' behind a
;; quote, and `(define-module (foo bar) ...)' inside a list.  Each shape is
;; read here, once, so that the language server, the outline, the completion
;; list and the rename all agree about what a name is and where it lives.
;;
;; Nothing here expands a macro, resolves a module import or reads a package
;; definition: a reference is matched by name, within one dialect.  That is the
;; same promise the rest of sift makes -- it reads the text you have -- and the
;; limits it implies are written down in the manual's table of what the
;; language server answers.

(define-module (sift definitions)
  ;; Not declarative, unlike the rest of the library: the language server hands
  ;; these accessors to `map' and stores them beside the definitions they
  ;; describe, and a declarative binding is only a call site the compiler may
  ;; inline -- used as a value it is an error rather than a procedure.
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:export (<definition>
            definition?
            make-definition
            definition-name
            definition-node
            definition-form
            definition-kind
            definition-scope
            definition-index
            definition-end
            definition-formals
            definition-docstring
            definition-forms
            binding-forms
            parse-definitions
            definitions-in
            definitions-by-name
            top-level-definition
            occurrences-of
            symbol-name
            symbol-node-at
            node-path-at
            resolve-definition
            resolve-name
            document-symbol-tree
            symbol-kind
            completion-kind))

;;; ===== What a definition is =====

;; A definition is a name, the atom that spells it, the form that defines it,
;; what kind of thing it is, and the form whose extent is the name's scope --
;; @code{#f} when the definition is one of the file's own top-level forms.
;; The index and the end are the atom's character range, kept beside the node
;; so that a caller holding the text can turn them into a protocol position
;; without walking the tree again.
(define-record-type <definition>
  (make-definition name node form kind scope index end formals)
  definition?
  (name definition-name)
  (node definition-node)
  (form definition-form)
  (kind definition-kind)
  (scope definition-scope)
  (index definition-index)
  (end definition-end)
  (formals definition-formals))

;;; ===== What defines a name =====

;; The operator, and what a form headed by it defines.  `method', `macro' and
;; the rest are only names for a kind; the language server maps each to the
;; number its protocol uses, and the outline uses the same word.
(define definition-forms
  '((function . (define define* define-public define-inlinable
                  defun defsubst define-minor-mode define-derived-mode
                  define-generic-mode cl-defun cl-defsubst
                  defgeneric cl-defgeneric))
    (macro . (define-syntax define-macro defmacro cl-defmacro))
    (method . (defmethod cl-defmethod))
    (variable . (defvar defvar-local defcustom defparameter defface))
    (constant . (defconst defconstant))
    (class . (defclass define-class))
    (struct . (defstruct cl-defstruct define-record-type))
    (type . (deftype define-condition))
    (package . (defpackage define-package))
    (module . (define-module))
    (alias . (defalias))))

;; The forms that bind a name in a scope rather than define one in a file.
;; They are a separate list because the answer is a jump to a parameter or a
;; `let' variable, not to a definition the outline should list.
(define binding-forms
  '(let let* letrec letrec* do lambda lambda* cl-lambda
     dolist dotimes multiple-value-bind))

(define (form-kind operator)
  "Return what a form headed by @var{operator} defines, or @code{#f}."
  (let loop ((entries definition-forms))
    (cond
      [(null? entries) #f]
      [(memq operator (cdr (car entries))) (car (car entries))]
      [else (loop (cdr entries))])))

;;; ===== Reading a form =====

(define (elements node)
  "Return the data elements of @var{node}; empty when it is not a list."
  (if (node-list? node) (node-elements node) '()))

(define (head-name node)
  (and (node-list? node) (node-head-name node)))

(define (arguments-of node)
  (let ((els (elements node)))
    (if (pair? els) (cdr els) '())))

(define (nth-or lst k)
  (if (and (>= k 0) (< k (length lst))) (list-ref lst k) #f))

(define (node-index node) (source-location-index (node-start node)))
(define (node-end-index node) (source-location-index (node-end node)))

(define (name-atom? node)
  (and node (node-atom? node) (eq? (node-atom-type node) 'symbol)))

(define (symbol-name node)
  "Return the name of an atom, or @code{#f} when it does not name anything.

A keyword loses its @code{#:} or @code{:} prefix and a package marker loses
its qualifier, so that `#:run', `app:run' and `run' compare the same way --
which is what a reference is."
  (and node
    (node-atom? node)
    (case [node-atom-type node]
      [(symbol keyword)
       (let* ((value (node-value node))
              (marker (node-package-marker node))
              (bare (cond
                      [(and marker (< (string-length marker) (string-length value)))
                       (substring value (string-length marker))]
                      [(string-prefix? "#:" value) (substring value 2)]
                      [(string-prefix? ":" value) (substring value 1)]
                      [else value])))
         bare)]
      [else #f])))

(define (bare-atom node)
  "Return @var{node} when it is an atom, or the atom a quote prefix holds.

`(defalias \\='f ...)' names `f', and a reader that stopped at the quote would
report the definition as a list with no name in it."
  (cond
    [(and node (node-atom? node)) node]
    [(and node (node-prefix? node) (node-atom? (node-child node)))
     (node-child node)]
    [else #f]))

(define (quoted? node)
  "Return @code{#t} when @var{node} is a prefix that quotes its child."
  (and node
    (node-prefix? node)
    (let ((token (node-token node)))
      (and (string? token)
        (member token '("'" "`" ",@" ","))
        #t))))

(define (definition-name-node form)
  "Return the atom that names what @var{form} defines, or @code{#f}.

The three shapes that are not \\\"first argument\\\" are read here: the argument
list of `(define (f x) ...)', the module spec of `(define-module (m) ...)' and
the quoted symbol of `(defalias \\='f ...)'."
  (let* ((operator (head-name form))
         (kind (and operator (form-kind operator)))
         (args (arguments-of form))
         (first (nth-or args 0)))
    (and kind
      (case kind
        [(module)
         (let ((inner (and first (node-list? first) (node-head first))))
           (and inner (node-atom? inner) inner))]
        [(function macro method)
         (cond
           [(and first (node-list? first) (node-head first))
            (let ((inner (node-head first)))
              (and inner (node-atom? inner) inner))]
           [else (bare-atom first)])]
        [else (bare-atom first)]))))

(define (parameter-marker? name)
  "Return @code{#t} for an argument marker such as @code{&optional}."
  (and name
    (or (string-prefix? "&" name)
      (string-prefix? "#:" name)
      (string-prefix? ":" name))))

(define (parameter-atoms node)
  "Return the parameter atoms of a formal list, destructuring included.

`(a b)' gives two, `((a b) &rest c)' gives three, and a marker such as
`&optional' names nothing and is skipped."
  (cond
    [(not node) '()]
    [(node-atom? node)
     (let ((name (symbol-name node)))
       (if (and name (not (parameter-marker? name))) (list node) '()))]
    [(node-list? node) (append-map parameter-atoms (elements node))]
    [else '()]))

(define (form-parameters form)
  "Return the parameter atoms a function definition form lists, or @code{'()}.

`(define (f a b) ...)' keeps them inside the argument list; `(defun f (a b))'
keeps them in the second argument; `(defmethod f ((x t)) ...)' likewise."
  (let* ((operator (head-name form))
         (args (arguments-of form))
         (first (nth-or args 0)))
    (cond
      [(memq operator '(define define* define-public define-inlinable))
       (if (and first (node-list? first))
         (append-map parameter-atoms (cdr (elements first)))
         '())]
      [(memq operator '(defun defmacro defsubst cl-defun cl-defmacro cl-defsubst
                              defgeneric cl-defgeneric defmethod cl-defmethod
                              define-minor-mode define-derived-mode
                              define-generic-mode))
       (parameter-atoms (nth-or args 1))]
      [else '()])))

(define (docstring-at node)
  "Return the text of @var{node} when it is a string literal, else @code{#f}."
  (and node
    (node-atom? node)
    (eq? (node-atom-type node) 'string)
    (node-value node)))

(define (definition-docstring definition)
  "Return the docstring a definition carries, or @code{#f}.

The docstring of a function is the string after its argument list; the
one of a variable is the string after its value; a `define' of a value has
no place to put one, which is why the case is written out here."
  (let* ((form (definition-form definition))
         (operator (and form (head-name form)))
         (args (and form (arguments-of form))))
    (and args
      (cond
        [(memq operator '(define define* define-public define-inlinable))
         (let ((first (nth-or args 0)))
           (if (and first (node-list? first))
             (docstring-at (nth-or args 1))
             #f))]
        [(memq operator '(defun defmacro defsubst cl-defun cl-defmacro cl-defsubst
                                defmethod cl-defmethod defgeneric cl-defgeneric
                                define-minor-mode define-derived-mode
                                define-generic-mode define-syntax))
         (docstring-at (nth-or args 2))]
        [(memq operator '(defvar defvar-local defcustom defparameter defface))
         (docstring-at (nth-or args 2))]
        [else #f]))))

(define (defining-kind operator kind form)
  "Return @var{kind}, refined when the form itself says more.

`(define x 1)' and `(define (f x) 1)' share an operator but not a kind, and an
outline that called both a function would be wrong about half the file."
  (cond
    [(and (memq operator '(define define* define-public define-inlinable))
       (let ((first (nth-or (arguments-of form) 0)))
         (not (and first (node-list? first)))))
     'variable]
    [else kind]))

(define (definition-of-form form scope)
  "Return @var{form} as a definition scoped by @var{scope}, or @code{#f}."
  (let* ((operator (head-name form))
         (kind (and operator (form-kind operator)))
         (name-node (and kind (definition-name-node form))))
    (and name-node
      (let ((name (symbol-name name-node)))
        (and name
          (make-definition name
                           name-node
                           form
                           (defining-kind operator kind form)
                           scope
                           (node-index name-node)
                           (node-end-index name-node)
                           (form-parameters form)))))))

;;; ===== Bindings =====

(define-record-type <binding>
  (make-binding name-node kind start end)
  binding?
  (name-node binding-node)
  (kind binding-kind)
  (start binding-start)
  (end binding-end))

(define (binding-name binding)
  (symbol-name (binding-node binding)))

(define (bindings-of-atoms atoms span kind)
  (append-map (lambda (node)
                (let ((name (bare-atom node)))
                  (if (and name (symbol-name name))
                    (list (make-binding name kind (car span) (cdr span)))
                    '())))
              atoms))

(define (binding-list-bindings node operator span)
  "Return the bindings a binding list introduces.

In a `let*' a binding is visible from its own end, which is what makes the
later initialisers see it; every other `let' family form sees all of them from
the body onwards."
  (if (not (and node (node-list? node)))
    '()
    (append-map
     (lambda (element)
       (let* ((name (bare-atom (if (node-list? element)
                                 (node-head element)
                                 element)))
              (visible (if (eq? operator 'let*)
                         (cons (node-end-index element) (cdr span))
                         span)))
         (if (and name (symbol-name name))
           (list (make-binding name 'variable (car visible) (cdr visible)))
           '())))
     (elements node))))

(define (form-bindings form)
  "Return the bindings @var{form} introduces, with the range each is visible in."
  (let* ((operator (head-name form))
         (args (arguments-of form))
         (end (node-end-index form))
         (at (lambda (k) (nth-or args k)))
         (span-from (lambda (node)
                      (if node (cons (node-index node) end) (cons end end)))))
    (case operator
      [(let let* letrec letrec*)
       ;; A named `let' is a loop: the name is a binding of the body, and the
       ;; binding list is the second argument rather than the first.
       (if (and (name-atom? (at 0)) (node-list? (at 1)))
         (cons (make-binding (at 0) 'function
                             (car (span-from (at 2))) (cdr (span-from (at 2))))
               (binding-list-bindings (at 1) 'let (span-from (at 2))))
         (binding-list-bindings (at 0) operator (span-from (at 1))))]
      [(do)
       (binding-list-bindings (at 0) 'do (span-from (at 1)))]
      [(lambda lambda* cl-lambda)
       (bindings-of-atoms (parameter-atoms (at 0)) (span-from (at 1))
                          'parameter)]
      [(defun defmacro defsubst cl-defun cl-defmacro cl-defsubst
              defgeneric cl-defgeneric defmethod cl-defmethod
              define-minor-mode define-derived-mode define-generic-mode)
       (bindings-of-atoms (parameter-atoms (at 1)) (span-from (at 2))
                          'parameter)]
      [(define define* define-public define-inlinable)
       (let ((first (at 0)))
         (if (and first (node-list? first))
           (bindings-of-atoms (append-map parameter-atoms
                                          (cdr (elements first)))
                              (cons (node-end-index first) end)
                              'parameter)
           '()))]
      [(dolist dotimes)
       (let* ((spec (at 0))
              (name (and spec (node-list? spec) (bare-atom (node-head spec)))))
         (if (and name (symbol-name name))
           (let ((span (span-from (at 1))))
             (list (make-binding name 'variable (car span) (cdr span))))
           '()))]
      [(multiple-value-bind)
       (let* ((spec (at 0))
              (atoms (if (and spec (node-list? spec)) (elements spec) '())))
         (bindings-of-atoms atoms (span-from (at 2)) 'variable))]
      [else '()])))

(define (internal-bindings form)
  "Return the bindings the definitions directly inside @var{form} introduce.

`(define (f) (define (g) 1) (g))' is the case: `g' is visible in the body it
sits in, and an editor that asked for the definition of the second `g' should
land on the first one and not go looking in another file."
  (append-map
   (lambda (element)
     (let ((definition (and (node-list? element) (definition-of-form element #f))))
       (if definition
         (list (make-binding (definition-node definition)
                             (definition-kind definition)
                             (node-index element)
                             (node-end-index form)))
         '())))
   (elements form)))

(define (bindings-in form)
  (append (form-bindings form) (internal-bindings form)))

;;; ===== Walking =====

(define (quoted-context? ancestors)
  (any quoted? ancestors))

(define (walk-forms proc ast)
  "Call @var{proc} on every form in @var{ast}, with its enclosing forms.

A form inside a quote is data and is not visited: `(quote (define (f) 1))' is
a list of three symbols, not a definition."
  (define (walk node ancestors)
    (unless (or (quoted? node) (node-datum-comment? node))
      (when (node-list? node) (proc node ancestors))
      (cond
        [(node-list? node)
         (for-each (lambda (child) (walk child (cons node ancestors)))
                   (elements node))]
        [(node-conditional? node)
         (let ((feature (node-feature node))
               (guarded (node-guarded-form node)))
           (when feature (walk feature ancestors))
           (when guarded (walk guarded ancestors)))])))
  (for-each (lambda (node) (when (node-data? node) (walk node '()))) ast))

(define (parse-definitions ast)
  "Return every definition in @var{ast}, outermost first, each with its scope.

The scope of a definition is the definition form it is written inside, or
@code{#f} when it is one of the file's own top-level forms; that is the parent
the outline nests under."
  (let ((found '()))
    (walk-forms (lambda (node ancestors)
                  (let ((scope (find (lambda (ancestor)
                                       (and (head-name ancestor)
                                         (form-kind (head-name ancestor))))
                                     ancestors)))
                    (let ((definition (definition-of-form node scope)))
                      (when definition
                        (set! found (cons definition found))))))
                ast)
    (sort-by-index (reverse found))))

(define (sort-by-index definitions)
  (sort definitions (lambda (a b) (< (definition-index a) (definition-index b)))))

(define (definitions-in ast)
  "Return the definitions of @var{ast}, as `parse-definitions' does."
  (parse-definitions ast))

(define (definitions-by-name ast name)
  "Return the definitions of @var{name} in @var{ast}, top level first."
  (filter (lambda (definition)
            (string=? (definition-name definition) name))
          (parse-definitions ast)))

(define (top-level-definition ast name)
  "Return the file-level definition of @var{name}, or @code{#f}."
  (let ((all (filter (lambda (definition)
                       (and (not (definition-scope definition))
                         (string=? (definition-name definition) name)))
                     (parse-definitions ast))))
    (and (pair? all) (car all))))

(define (document-symbol-tree ast)
  "Return the definitions of @var{ast} nested the way the outline shows them.

A definition inside another one is a child of it, nearest first, which is what
makes a `define' inside a `let' appear under the function that holds it rather
than beside it."
  (let* ((definitions (parse-definitions ast))
         (top (filter (lambda (definition) (not (definition-scope definition)))
                      definitions)))
    (map (lambda (definition)
           (cons definition
                 (map (lambda (child) (cons child '()))
                      (filter (lambda (child)
                                (eq? (definition-scope child)
                                     (definition-form definition)))
                              definitions))))
         top)))

;;; ===== References =====

(define (occurrences-of ast name)
  "Return every symbol atom in @var{ast} called @var{name}, in source order.

A quote is not entered, because `'(a b)' is a list of data; a datum comment is
not entered either, for the reason the reader was told not to read it."
  (let ((found '()))
    (define (walk node)
      (cond
        [(node-datum-comment? node) #f]
        [(node-prefix? node)
         ;; `'x' names the same symbol as `x', so a quote prefix passes
         ;; through to its child and nothing else.
         (let ((child (node-child node)))
           (when (and child (not (quoted? node))) (walk child)))]
        [(node-atom? node)
         (when (and (eq? (node-atom-type node) 'symbol)
                 (string=? (node-value node) name))
           (set! found (cons node found)))]
        [(node-list? node)
         (for-each walk (node-children node))]
        [(node-conditional? node)
         (let ((feature (node-feature node))
               (guarded (node-guarded-form node)))
           (when feature (walk feature))
           (when guarded (walk guarded)))]
        [else #f]))
    (for-each walk ast)
    (sort (reverse found) (lambda (a b) (< (node-index a) (node-index b))))))

;;; ===== Resolving a position =====

(define (inside? node index)
  (and (<= (node-index node) index)
    (< index (max (node-index node) (node-end-index node)))))

(define (child-at node index)
  (find (lambda (child) (and (node-data? child) (inside? child index)))
        (node-children node)))

(define (node-path-at ast index)
  "Return two values: the innermost node at @var{index}, and its ancestors."
  (define (descend node ancestors)
    (let ((child (and (node-list? node) (child-at node index))))
      (if child
        (descend child (cons node ancestors))
        (values node ancestors))))
  (let loop ((nodes ast))
    (cond
      [(null? nodes) (values #f '())]
      [(and (node-data? (car nodes)) (inside? (car nodes) index))
       (descend (car nodes) '())]
      [else (loop (cdr nodes))])))

(define (symbol-node-at ast index)
  "Return the symbol atom at @var{index}, or @code{#f}."
  (call-with-values (lambda () (node-path-at ast index))
    (lambda (node ancestors)
      (and node (node-atom? node)
        (eq? (node-atom-type node) 'symbol)
        node))))

(define (binding-definition binding scope)
  "Return @var{binding} as a definition located where it is written."
  (make-definition (binding-name binding)
                   (binding-node binding)
                   scope
                   (binding-kind binding)
                   scope
                   (node-index (binding-node binding))
                   (node-end-index (binding-node binding))
                   '()))

(define (resolve-in-scope ancestors name index)
  "Return the binding of @var{name} that @var{index} is inside, or @code{#f}."
  (let loop ((forms ancestors))
    (if (null? forms)
      #f
      (let* ((form (car forms))
             (binding
              (find (lambda (candidate)
                      (and (string=? (or (binding-name candidate) "") name)
                        (<= (binding-start candidate) index)
                        (< index (binding-end candidate))))
                    (bindings-in form))))
        (or (and binding (binding-definition binding form))
          (loop (cdr forms)))))))

(define (resolve-name ast index)
  "Return the name the symbol at @var{index} has, or @code{#f}."
  (let ((node (symbol-node-at ast index)))
    (and node (symbol-name node))))

(define (self-binding ancestors node)
  "Return the binding @var{node} names, when it is the name of one.

A request on the `x' of `(lambda (x) ...)' is a request about that binding,
and an editor that answered \"nothing here\" would be wrong about the one
place the name is most obviously defined."
  (find (lambda (binding) (eq? (binding-node binding) node))
        (append-map bindings-in ancestors)))

(define (resolve-definition ast index)
  "Return the definition the symbol at @var{index} names, or @code{#f}.

The name itself first, then lexical scope, then the file's own definitions: a
parameter or a `let' variable is nearer than a definition of the same name,
and a reference to a function defined later in the file is still a reference
to it."
  (let ((node (symbol-node-at ast index)))
    (and node
      (let ((name (symbol-name node)))
        (call-with-values (lambda () (node-path-at ast index))
          (lambda (cursor ancestors)
            (let ((self (self-binding ancestors node)))
              (cond
                [self (binding-definition self (car ancestors))]
                [else (or (resolve-in-scope ancestors name index)
                        (top-level-definition ast name))]))))))))

;;; ===== What an editor calls a symbol =====

;; LSP's SymbolKind: Function 12, Method 6, Variable 13, Constant 14, Class 5,
;; Struct 23, TypeParameter 26, Module 2, Package 4, Alias is a Function.
(define (symbol-kind kind)
  "Return the protocol's number for @var{kind}."
  (case kind
    [(function alias) 12]
    [(method) 6]
    [(macro) 12]
    [(variable parameter) 13]
    [(constant) 14]
    [(class) 5]
    [(struct) 23]
    [(type) 26]
    [(module) 2]
    [(package) 4]
    [else 13]))

;; LSP's CompletionItemKind: Function 3, Variable 6, Class 7, Module 9,
;; Constant 21, Struct 22.
(define (completion-kind kind)
  "Return the completion kind for @var{kind}."
  (case kind
    [(function alias method macro) 3]
    [(variable parameter) 6]
    [(constant) 21]
    [(class) 7]
    [(struct) 22]
    [(module package) 9]
    [(type) 25]
    [else 6]))
