;; sift --- Scheme checkers
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Checkers for idioms that read one way in Scheme and another in the other
;; two dialects: internal definitions, an `if' that is really a `when', a
;; `letrec' that is really a `let', and a quasiquote that unquotes a constant.

(define-module (sift checkers scheme)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift format)
  #:use-module (sift imports)
  #:use-module (sift check base)
  #:export (scheme-checkers
            internal-define-checker
            if-without-else-checker
            letrec-single-binding-checker
            quasiquote-unquote-constant-checker
            clause-brackets-checker
            docstring-texinfo-checker
            unused-binding-checker
            unused-import-checker
            library-not-found-checker))

;;; ===== internal-define =====

(define (procedure-form? node)
  "Return @code{#t} when @var{node} introduces a procedure body.

That is a @code{lambda}, or a @code{define} whose first argument is the
@code{(name . formals)} pair.  A body of either may start with definitions:
Scheme has always allowed that, and an internal definition outside one is a
definition that will run in the wrong scope."
  (cond
    [(list-headed-by? node 'lambda) #t]
    [(list-headed-by? node 'define)
     (let ((first (and (pair? (form-arguments node)) (car (form-arguments node)))))
       (and first (node-list? first)))]
    [else #f]))

(define (body-of owner)
  "Return the body forms of a procedure @var{owner}.

A docstring is not a body form, so it is dropped: a definition after the
docstring is still at the head of the body, which is where Scheme allows it."
  (cond
    [(list-headed-by? owner 'lambda) (body-elements owner 1)]
    [(list-headed-by? owner 'define) (body-elements owner 2)]
    [else '()]))

(define (definition-before? form)
  "Return @code{#t} when @var{form} is itself a definition.

A Scheme body may begin with any number of definitions, and that is not the
problem this rule is about: the problem is a definition that comes after the
first expression, where it silently gets the wrong scope."
  (and (node-list? form) (list-headed-by? form 'define)))

(define (internal-define-warnings ast source dialect)
  (let ((warnings '()))
    (walk-with-ancestors
     (lambda (node ancestors)
       (when (list-headed-by? node 'define)
         ;; The nearest enclosing form decides: a `let` in between is the
         ;; nested scope the author meant, so the definition is fine.
         (let ((owner
                (find (lambda (ancestor)
                        (or (procedure-form? ancestor)
                          (head-is-one-of? ancestor
                                           '(let let* letrec letrec*
                                              parameterize))))
                      ancestors)))
           (when (and owner (procedure-form? owner))
             (let* ((body (body-of owner))
                    (index (list-index (lambda (form) (eq? form node)) body))
                    (before (and index (take body index))))
               ;; at the head of the body a definition is exactly what Scheme
               ;; allows; after an expression it is a `let` that was written
               ;; as a `define`
               (when (and index
                       (> index 0)
                       (any (lambda (form) (not (definition-before? form)))
                            before))
                 (set! warnings
                       (cons (make-lint-warning 'internal-define 'warning
                                                "`define` inside a procedure body, after the first expression"
                                                (node-start node))
                             warnings))))))))
     ast)
    (reverse warnings)))

(define internal-define-checker
  (checker 'internal-define
           "`define` inside a procedure body, after the first expression"
           '(scheme)
           internal-define-warnings
           #:severity 'warning
           #:example '((input . "(define (f)\n  (define y 1)\n  (if y 2 3))")
                       (fix . "no automatic fix; use `let' for a local binding"))))

;;; ===== if-without-else =====

(define (if-without-else-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (when (and (list-headed-by? node 'if)
               (= 2 (argument-count node)))
         (let ((head (node-head node)))
           (set! warnings
                 (cons (make-lint-warning/range
                        'if-without-else 'warning
                        "`if` has no else branch; `when` says it once"
                        (node-start head)
                        (node-index head) (node-end-index head)
                        (lambda (source start end) "when"))
                       warnings)))))
     ast)
    (reverse warnings)))

(define if-without-else-checker
  (checker 'if-without-else
           "`if` with no else branch, where `when` expresses the intent"
           '(scheme)
           if-without-else-warnings
           #:severity 'warning
           #:example '((input . "(if (ready?) (go))")
                       (fix . "(when (ready?) (go))"))))

;;; ===== letrec-single-binding =====

(define (tree-contains-symbol? node name)
  (let ((found #f))
    (for-each-node
     (lambda (candidate)
       (when (symbol-atom? candidate name) (set! found #t)))
     (list node))
    found))

(define (letrec-single-binding-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (when (head-is-one-of? node '(letrec letrec*))
         (let* ((args (form-arguments node))
                (bindings (and (pair? args) (car args))))
           (when (and (node-list? bindings)
                   (= 1 (length (node-elements bindings))))
             (let* ((binding (car (node-elements bindings)))
                    (parts (if (node-list? binding) (form-elements binding) '()))
                    (name (and (pair? parts) (atom-symbol (car parts))))
                    (init (and (pair? parts) (pair? (cdr parts))
                            (cadr parts))))
               (when (and name init
                       (not (tree-contains-symbol? init name)))
                 (set! warnings
                       (cons (make-lint-warning
                              'letrec-single-binding 'info
                              (format #f "`~a` with one binding that is not recursive; use `let`"
                                      (node-head-name node))
                              (node-start node))
                             warnings))))))))
     ast)
    (reverse warnings)))

(define letrec-single-binding-checker
  (checker 'letrec-single-binding
           "`letrec` with a single binding that does not need recursive scope"
           '(scheme)
           letrec-single-binding-warnings
           #:severity 'info
           #:example '((input . "(letrec ((x 1)) x)")
                       (fix . "no automatic fix; `let` expresses the same thing"))))

;;; ===== quasiquote-unquote-constant =====

(define (constant-atom? node)
  (and (node-atom? node)
    (memq (node-atom-type node) '(number string keyword boolean character))))

(define (quasiquote-unquote-constant-warnings ast source dialect)
  (let ((warnings '()))
    ;; Three spellings mean the same thing: the prefix nodes a reader builds
    ;; for ``, ` and `,`, and the long-hand `(quasiquote ...)' /
    ;; `(unquote ...)' forms a macro might generate.
    (define (report node)
      (set! warnings
            (cons (make-lint-warning
                   'quasiquote-unquote-constant 'info
                   "a constant is unquoted inside a quasiquote, where quoting it would do"
                   (node-start node))
                  warnings)))
    (define (check-quasiquote inner)
      (cond
        [(and (node-prefix? inner) (equal? (node-token inner) ","))
         (let ((child (node-child inner)))
           (when (and child (constant-atom? child)) (report inner)))]
        [(list-headed-by? inner 'unquote)
         (let ((args (form-arguments inner)))
           (when (and (pair? args) (constant-atom? (car args))) (report inner)))]
        [(node-list? inner)
         (for-each check-quasiquote (node-elements inner))]))
    (for-each-node
     (lambda (node)
       (cond
         [(and (node-prefix? node) (equal? (node-token node) "`"))
          (let ((child (node-child node)))
            (when child (check-quasiquote child)))]
         [(list-headed-by? node 'quasiquote)
          (let ((args (form-arguments node)))
            (when (pair? args) (check-quasiquote (car args))))]))
     ast)
    (reverse warnings)))

(define quasiquote-unquote-constant-checker
  (checker 'quasiquote-unquote-constant
           "`(quasiquote (unquote constant))` can be simplified"
           '(scheme)
           quasiquote-unquote-constant-warnings
           #:severity 'info
           #:example '((input . "`(1 ,2)")
                       (fix . "no automatic fix; write the constant directly"))))

;;; ===== clause-brackets =====

(define (bracket-pair style)
  (case style
    [(square) (cons "[" "]")]
    [(parens) (cons "(" ")")]
    [else #f]))

(define (clause-brackets-warnings ast source dialect)
  (let* ((style (or (option 'brackets #f) (default-brackets dialect)))
         (wanted (bracket-pair style))
         (warnings '()))
    ;; `check` is defined at the top of the body, not inside the `when`:
    ;; a `when` body is an expression, and a definition there is not allowed.
    (define (check clause)
      (when (and (node-list? clause) (node-close clause))
        (let ((open (node-open clause))
              (close (node-close clause)))
          ;; only a bracket that is really a bracket can be swapped: a
          ;; `#(' vector or a `{' brace is not a clause list
          (when (and (or (equal? open "(") (equal? open "["))
                  (or (equal? close ")") (equal? close "]"))
                  (not (equal? open (car wanted))))
            (set! warnings
                  (cons (make-lint-warning/range 'clause-brackets 'warning
                                                 (format #f "`~a` clause is written with ~a; this project writes it with ~a"
                                                         (or (node-head-name (node-head clause)) 'clause)
                                                         (if (equal? open "(") "parentheses" "square brackets")
                                                         (if (equal? open "(") "square brackets" "parentheses"))
                                                 (node-start clause)
                                                 (node-index clause) (node-end-index clause)
                                                 (lambda (source start end)
                                                   (string-append (car wanted)
                                                                  (substring source (+ start 1) (- end 1))
                                                                  (cdr wanted))))
                        warnings))))))
    (when wanted
      (for-each-form
       (lambda (node)
         (when (and (node-list? node)
                 (memq (node-head-name node) '(cond case)))
           ;; the clauses are the children of the form, not the form itself
           (for-each check (form-arguments node))))
       ast))
    (reverse warnings)))

(define clause-brackets-checker
  (checker 'clause-brackets
           "A `cond` or `case` clause written with the other kind of bracket"
           '(scheme)
           clause-brackets-warnings
           #:severity 'warning
           #:example '((input . "(cond ((= x 1) 'one)\n      (else 'other))")
                       (fix . "(cond [(= x 1) 'one]\n      [else 'other])"))))

;;; ===== docstring-texinfo =====

(define (scheme-parameters node)
  "Return the parameter names of a procedure definition, as strings."
  (let* ((arguments (form-arguments node))
         (first (and (pair? arguments) (car arguments)))
         (formals (cond
                    [(and first (node-list? first)) first]
                    [else #f])))
    ;; the first element of a `(define (name . formals) ...)' head is the
    ;; procedure's name, not a parameter
    (if formals
      (append-map (lambda (element)
                    (let ((name (and (node-atom? element) (node-value element))))
                      (if (and name (not (string-contains name ".")))
                        (list name)
                        '())))
                  (if (pair? (node-elements formals))
                    (cdr (node-elements formals))
                    '()))
      '())))

(define (docstring-texinfo-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (when (node-list? node)
         (let ((doc (docstring-node node))
               (parameters (scheme-parameters node)))
           (when (and doc (pair? parameters))
             (let ((text (node-value doc)))
               (for-each
                (lambda (parameter)
                  (unless (string-contains text
                                           (string-append "@var{" parameter "}"))
                    (set! warnings
                          (cons (make-lint-warning 'docstring-texinfo 'info
                                                   (format #f "the docstring does not document `@var{~a}`"
                                                           parameter)
                                                   (node-start doc))
                                warnings))))
                parameters))))))
     ast)
    (reverse warnings)))

(define docstring-texinfo-checker
  (checker 'docstring-texinfo
           "A Guile docstring that does not document its arguments with `@var{}`"
           '(scheme)
           docstring-texinfo-warnings
           #:severity 'info
           #:example '((input . "(define (f x) \"Return X.\" x)")
                       (fix . "(define (f x) \"Return @var{x}.\" x)"))))

;;; ===== unused-binding =====

(define binding-form-heads
  '(let let* letrec letrec* let-values let*-values))

(define (binding-pair-names binding)
  "Return the names a @var{binding} binds, as (name-node . text) pairs.

A binding is `(name value)', `((a b) value)' in `let-values', or a bare name
in a `do' or a `let' that only declares."
  (cond
    [(node-atom? binding)
     (if (eq? (node-atom-type binding) 'symbol)
       (list (cons binding (node-value binding)))
       '())]
    [(node-list? binding)
     (let ((head (node-head binding)))
       (cond
         [(and head (node-atom? head) (eq? (node-atom-type head) 'symbol))
          (list (cons head (node-value head)))]
         ;; a list of formals: take the symbols out of it, one level deep
         [(node-list? head) (formals-names head)]
         [else '()]))]
    [else '()]))

(define (formals-names formals)
  "Return the symbol names in a formal list, markers and destructuring aside."
  (append-map
   (lambda (element)
     (cond
       [(and (node-atom? element)
          (eq? (node-atom-type element) 'symbol)
          (not (string-prefix? "&" (node-value element))))
        (list (cons element (node-value element)))]
       [(node-list? element) (formals-names element)]
       [else '()]))
   (node-elements (if (node-list? formals) formals '()))))

(define (binding-list-of form)
  "Return the binding list of a @var{form}, or @code{#f}."
  (let ((arguments (form-arguments form)))
    (and (pair? arguments) (car arguments))))

(define (ignored-name? name)
  "Return @code{#t} for a name that says it is not meant to be used."
  (or (string-prefix? "_" name)
    (string=? name "-")
    (string=? name "ignored")))

(define (symbol-uses form name)
  "Return how many times the symbol @var{name} appears under @var{form}."
  (fold-nodes
   (lambda (node count)
     (if (and (node-atom? node)
           (eq? (node-atom-type node) 'symbol)
           (string=? (node-value node) name))
       (+ count 1)
       count))
   0
   (list form)))

(define (unused-binding-warnings ast source dialect)
  (let ((warnings '()))
    (walk-with-ancestors
     (lambda (node ancestors)
       (when (and (code-position? node ancestors)
               (node-list? node)
               (memq (node-head-name node) binding-form-heads))
         (let ((bindings (binding-list-of node)))
           (when (node-list? bindings)
             (for-each
              (lambda (binding)
                (for-each
                 (lambda (pair)
                   (let ((name (cdr pair)))
                     (unless (ignored-name? name)
                       ;; the defining occurrence counts once; anything
                       ;; else is a use, which is what makes `set!' and a
                       ;; recursive reference count
                       (when (< (symbol-uses node name) 2)
                         (set! warnings
                               (cons (make-lint-warning 'unused-binding
                                                        'warning
                                                        (format #f
                                                                "`~a` is bound here and never used"
                                                                name)
                                                        (node-start (car pair)))
                                     warnings))))))
                 (binding-pair-names binding)))
              (node-elements bindings))))))
     ast)
    (reverse warnings)))

(define unused-binding-checker
  (checker 'unused-binding
           "A binding whose name is never referenced in the form that binds it"
           '(scheme elisp cl)
           unused-binding-warnings
           #:severity 'warning
           #:example '((input . "(let ((x 1) (y 2)) x)")
                       (fix . "no automatic fix; remove the binding or use it"))))

;;; ===== unused-import =====
;;
;; An import is a claim: "this file uses what this library exports".  sift can
;; only check the claims a file spells out -- the names in an `only' clause and
;; the pairs in a `rename' -- plus a whole library whose own `(export ...)'
;; sift can read, either in this file or in one beside it.  Everything else,
;; which is every plain `(import (some lib))' from a library sift cannot see,
;; is left alone: a rule that guessed there would fire on every file that
;; imports more than it uses, which is most of them.

(define (source-file-of ast)
  "Return the file @var{ast} was read from, or @code{#f} when it was a string."
  (let ((nodes (filter node-data? ast)))
    (and (pair? nodes) (source-location-file (node-start (car nodes))))))

(define (import-name-string node)
  "Return the name an atom in an import spec spells."
  (and node (node-atom? node) (node-value node)))

(define (import-label spec)
  "Return the library @var{spec} imports, ready to print."
  (let ((name (spec-library spec)))
    (and name (format #f "(~a)" (library-key name)))))

(define (single-line text)
  "Return @var{text} with every run of whitespace collapsed to one space.

A finding is printed on one line -- that is what an editor's problem list and
a `grep' both want -- and an import spec written across three lines is still
one spec."
  (let loop ((characters (string->list text)) (space? #f) (out '()))
    (cond
      [(null? characters) (list->string (reverse out))]
      [(char-whitespace? (car characters))
       (loop (cdr characters) #t out)]
      [else
        (loop (cdr characters) #f
              (if (and space? (pair? out))
                (cons (car characters) (cons #\space out))
                (cons (car characters) out)))])))

(define (line-end-of source index)
  "Return the index a deletion should stop at, given the end of a removed form.

A form alone on its line takes the line with it: an import the whole fix
deletes should not leave the blank line it was written on.  Anything else
after the form on that line is the author's and stops the deletion."
  (let loop ((at index))
    (cond
      [(>= at (string-length source)) at]
      [(char=? (string-ref source at) #\newline) (+ at 1)]
      [(memv (string-ref source at) '(#\space #\tab)) (loop (+ at 1))]
      [else index])))

(define (source-text source node)
  "Return @var{node} as it is written, on one line."
  (single-line (substring source (node-index node) (node-end-index node))))

(define (library-exports-of key ast project)
  "Return what @var{key} exports, from this file first and the project second."
  (or (assoc-ref (declared-libraries ast) key)
    (assoc-ref project key)))

(define (report-unused-spec form specs spec source warnings)
  "Report @var{spec} as unused, cutting the form when it is the only spec.

A form left with no spec would be `(import)', which no Scheme reads, so the
last spec takes the form with it -- one finding, one deletion, and a file that
still parses."
  (let* ((alone? (= 1 (length specs)))
         (node (if alone? form spec))
         (span (if alone?
                 (cons (node-index form) (line-end-of source (node-end-index form)))
                 (or (deletion-range specs spec)
                   (cons (node-index spec) (node-end-index spec))))))
    (cons (make-lint-warning/range
           'unused-import 'info
           (format #f "`~a` imports nothing this file uses"
                   (source-text source node))
           (node-start node) (car span) (cdr span)
           (lambda (text from to) ""))
          warnings)))

(define (unused-import-warnings ast source dialect)
  (let* ((used (referenced-names ast))
         (file (source-file-of ast))
         (project (project-libraries (and file (dirname file))))
         (warnings '()))
    (for-each
     (lambda (form)
       (let ((specs (import-specs form)))
         (for-each
          (lambda (spec)
            (case [spec-kind spec]
              [(only)
               (let* ((names (spec-name-nodes spec))
                      (unused (remove (lambda (node)
                                        (used? used (import-name-string node)))
                                      names)))
                 (cond
                   [(null? unused) #f]
                   ;; every name unused: the clause itself is the finding, and
                   ;; the fix deletes it rather than leaving `(only lib)'.
                   [(= (length unused) (length names))
                    (set! warnings (report-unused-spec form specs spec source warnings))]
                   [else
                     (for-each
                      (lambda (node)
                        (let ((range (deletion-range (cddr (data-elements spec)) node)))
                          (when range
                            (set! warnings
                                  (cons (make-lint-warning/range
                                         'unused-import 'info
                                         (format #f "`~a` is imported from `~a` and never used"
                                                 (import-name-string node)
                                                 (import-label spec))
                                         (node-start node) (car range) (cdr range)
                                         (lambda (source from to) ""))
                                        warnings)))))
                      unused)]))]
              [(rename)
               (for-each
                (lambda (pair)
                  (let* ((elements (data-elements pair))
                         (fresh (and (>= (length elements) 2) (list-ref elements 1)))
                         (name (and fresh (import-name-string fresh))))
                    (unless (used? used name)
                      (let ((range (deletion-range (cddr (data-elements spec)) pair)))
                        (when range
                          (set! warnings
                                (cons (make-lint-warning/range
                                       'unused-import 'info
                                       (format #f "`~a` is imported from `~a` under a new name and never used"
                                               name (import-label spec))
                                       (node-start pair) (car range) (cdr range)
                                       (lambda (source from to) ""))
                                      warnings)))))))
                (rename-pair-nodes spec))]
              [(library)
               (let* ((key (library-key (spec-library spec)))
                      (exports (library-exports-of key ast project)))
                 ;; only a library whose exports sift can read can be called
                 ;; unused as a whole, and only when none of them is used
                 (when (and (pair? exports)
                         (not (any (lambda (name) (used? used name)) exports)))
                   (set! warnings (report-unused-spec form specs spec source warnings))))]
              [else #f]))
          specs)))
     (import-forms ast))
    (reverse warnings)))

(define unused-import-checker
  (checker 'unused-import
           "A name or a library an `import` brings in that the file never uses"
           '(scheme)
           unused-import-warnings
           #:severity 'info
           #:example '((input . "(import (only (srfi 1) fold car))\n(display (fold + 0 '(1 2)))")
                       (fix . "delete `car` from the `only` clause"))))

;;; ===== library-not-found =====

(define (library-not-found-warnings ast source dialect)
  (let* ((declared (declared-libraries ast))
         (file (source-file-of ast))
         (project (project-libraries (and file (dirname file))))
         (known (append declared project))
         (warnings '()))
    ;; With no library anywhere to compare against there is nothing to say:
    ;; a project whose libraries live outside its own tree would otherwise get
    ;; a finding for every import it has.
    (when (pair? known)
      (for-each
       (lambda (form)
         (for-each
          (lambda (spec)
            (let* ((name (spec-library spec))
                   (node (spec-library-node spec))
                   (key (and name (library-key name))))
              (when (and node key
                      (not (well-known-library? name))
                      (not (assoc-ref known key)))
                (set! warnings
                      (cons (make-lint-warning
                             'library-not-found 'warning
                             (format #f "`~a` is imported but no file of this project declares it"
                                     (import-label spec))
                             (node-start node))
                            warnings)))))
          (import-specs form)))
       (import-forms ast)))
    (reverse warnings)))

(define library-not-found-checker
  (checker 'library-not-found
           "An `import` of a library that neither the file nor the project declares"
           '(scheme)
           library-not-found-warnings
           #:severity 'warning
           #:example '((input . "(import (my helpers))\n")
                       (fix . "no automatic fix; name the library the project declares, or add it"))))

;;; ===== The list =====

(define scheme-checkers
  (list internal-define-checker
        if-without-else-checker
        letrec-single-binding-checker
        quasiquote-unquote-constant-checker
        clause-brackets-checker
        docstring-texinfo-checker
        unused-binding-checker
        unused-import-checker
        library-not-found-checker))
