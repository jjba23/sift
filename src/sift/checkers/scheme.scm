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
  #:use-module (sift strings)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift format)
  #:use-module (sift imports)
  #:use-module (sift check base)
  #:export (scheme-checkers
            internal-define-checker
            if-without-else-checker
            letrec-single-binding-checker
            letrec-init-reference-checker
            quasiquote-unquote-constant-checker
            quasiquote-dotted-unquote-checker
            clause-brackets-checker
            docstring-texinfo-checker
            unused-binding-checker
            unused-import-checker
            library-not-found-checker
            unused-definition-checker))

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
           '(scheme racket)
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
           '(scheme racket)
           letrec-single-binding-warnings
           #:severity 'info
           #:example '((input . "(letrec ((x 1)) x)")
                       (fix . "no automatic fix; `let` expresses the same thing"))))

;;; ===== letrec-init-reference =====
;;;
;;; R7RS says an initializer of `letrec' may not refer to the value of any of
;;; the variables the form is binding: when it runs, none of them has been
;;; assigned yet.  All-lambda initializers satisfy that automatically, which is
;;; why the common use of `letrec' is a set of mutually recursive procedures;
;;; an initializer that is not a lambda and that mentions a binding is a
;;; `letrec*' written with the wrong name.

(define (letrec-bindings node)
  "Return the @code{(name . initializer)} pairs of @var{node}, or @code{'()}."
  (let ((arguments (form-arguments node)))
    (filter-map
     (lambda (binding)
       (let ((parts (form-elements binding)))
         (and (pair? parts) (pair? (cdr parts))
           (let ((name (atom-symbol (car parts))))
             (and name (cons name (cadr parts)))))))
     (or (and (pair? arguments) (node-list? (car arguments))
           (node-elements (car arguments)))
       '()))))

(define (letrec-init-reference-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (when (list-headed-by? node 'letrec)
         (let* ((bindings (letrec-bindings node))
                (names (map car bindings)))
           (for-each
            (lambda (binding)
              (let ((init (cdr binding)))
                ;; a lambda body runs later, when every binding is assigned
                (unless (and (node-list? init)
                          (eq? (node-head-name init) 'lambda))
                  (let ((referred (find (lambda (name)
                                          (tree-contains-symbol? init name))
                                        names)))
                    (when referred
                      (set! warnings
                            (cons (make-lint-warning
                                   'letrec-init-reference 'warning
                                   (format #f "this initializer refers to `~a`, which `letrec` has not assigned yet; `letrec*` evaluates them in order"
                                           referred)
                                   (node-start init))
                                  warnings)))))))
            bindings))))
     ast)
    (reverse warnings)))

(define letrec-init-reference-checker
  (checker 'letrec-init-reference
           "A `letrec` initializer that refers to a binding the form has not assigned yet"
           '(scheme)
           letrec-init-reference-warnings
           #:severity 'warning
           #:example '((input . "(letrec ((a 1) (b (+ a 1))) b)")
                       (fix . "(letrec* ((a 1) (b (+ a 1))) b)"))))

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
           '(scheme racket)
           quasiquote-unquote-constant-warnings
           #:severity 'info
           #:example '((input . "`(1 ,2)")
                       (fix . "no automatic fix; write the constant directly"))))

;;; ===== quasiquote-dotted-unquote =====
;;;
;;; Inside a quasiquote, `(key . ,value)' and `(key unquote value)' are the
;;; same list: a dotted pair whose tail is `(unquote value)' is a list of
;;; three elements, whichever way it was read.  The dot is the spelling an
;;; alist entry gets when its value is computed, and it reads as two halves
;;; where the second one is the whole rest; writing `unquote' out says which
;;; element is the variable.

(define (dotted-unquote-entry node)
  "Return the unquote that tails a @code{(key . ,value)} pair, or @code{#f}."
  (let ((elements (form-elements node)))
    (and (= 3 (length elements))
      (let ((dot (cadr elements))
            (unquote (caddr elements)))
        (and (node-atom? dot)
          (string=? (node-value dot) ".")
          (node-prefix? unquote)
          (equal? (node-token unquote) ",")
          (node-child unquote)
          unquote)))))

(define (quasiquote-ancestor? ancestors)
  "Return @code{#t} when a quasiquote encloses a node, with no unquote between."
  (let loop ((rest ancestors))
    (cond
      [(null? rest) #f]
      [(or (and (node-prefix? (car rest))
             (equal? (node-token (car rest)) "`"))
         (list-headed-by? (car rest) 'quasiquote))
       #t]
      [(or (and (node-prefix? (car rest))
             (equal? (node-token (car rest)) ","))
         (list-headed-by? (car rest) 'unquote))
       #f]
      [else (loop (cdr rest))])))

(define (quasiquote-dotted-unquote-warnings ast source dialect)
  (let ((warnings '()))
    (walk-with-ancestors
     (lambda (node ancestors)
       (let ((unquote (and (quasiquote-ancestor? ancestors)
                        (dotted-unquote-entry node))))
         (when unquote
           (let* ((dot (cadr (form-elements node)))
                  (start (node-index dot))
                  (comma-end (+ (node-index unquote)
                                (string-length (node-token unquote))))
                  (value (node-child unquote))
                  ;; The space after the comma is part of what the rewrite
                  ;; replaces: `unquote' brings its own, and keeping the
                  ;; author's as well is the double space `sift format' would
                  ;; take out.  A line break after the comma is structure and
                  ;; stays, so only whitespace on the same line is consumed.
                  (same-line? (not (memv #\newline
                                         (string->list
                                          (substring source comma-end
                                                     (node-index value))))))
                  (end (if same-line? (node-index value) comma-end))
                  (replacement (if same-line? "unquote " "unquote"))
                  ;; a comment between the dot and the comma is the author's,
                  ;; and a rewrite that spans it would delete it
                  (fix (and (string-every char-whitespace?
                                          (substring source
                                                     (node-end-index dot)
                                                     (node-index unquote)))
                         (lambda (text from to) replacement))))
             (set! warnings
                   (cons (make-lint-warning/range
                          'quasiquote-dotted-unquote 'warning
                          "a dotted pair with an unquote in its tail, which `unquote` written out says the same way"
                          (node-start node) start end fix)
                         warnings))))))
     ast)
    (reverse warnings)))

(define quasiquote-dotted-unquote-checker
  (checker 'quasiquote-dotted-unquote
           "A `(key . ,value)` pair inside a quasiquote, which is the same as `(key unquote value)`"
           '(scheme racket)
           quasiquote-dotted-unquote-warnings
           #:severity 'warning
           #:example '((input . "`((key . ,value))")
                       (fix . "`((key unquote value))"))))

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
         ;; `match' clauses are bracket-writing the same way a `cond' clause
         ;; is, and Racket writes both with a square one.  `syntax-case' is
         ;; left out because its second element is a list of literals rather
         ;; than a clause.
         (when (and (node-list? node)
                 (memq (node-head-name node)
                       '(cond case match match* match-lambda
                              match-lambda*)))
           ;; The clauses are the arguments after the ones that are not
           ;; clauses: a `case' key and a `match' subject are expressions,
           ;; and bracketing one would change the program.
           (for-each check
                     (drop (form-arguments node)
                           (min (length (form-arguments node))
                                (clause-leading-arguments
                                 (node-head-name node)))))))
       ast))
    (reverse warnings)))

(define clause-brackets-checker
  (checker 'clause-brackets
           "A `cond` or `case` clause written with the other kind of bracket"
           '(scheme racket)
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
                      (if (and name (not (string-find name ".")))
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
                  (unless (string-find text
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
    (walk-forms
     (lambda (node ancestors)
       (when (and (node-list? node)
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
           '(scheme racket elisp cl)
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
;;
;; What the fix deletes it deletes for a reason.  A name goes and the space
;; before it goes, a spec goes and the space before it goes, and when every
;; spec of a form is unused the *form* goes -- because a fix that removed the
;; last spec would leave `(import)', which no Scheme reads.

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

(define (source-file-of ast)
  "Return the file @var{ast} was read from, or @code{#f} when it was a string."
  (let ((nodes (filter node-data? ast)))
    (and (pair? nodes) (source-location-file (node-start (car nodes))))))

(define (library-exports-of key ast project)
  "Return what @var{key} exports, from this file first and the project second."
  (or (assoc-ref (declared-libraries ast) key)
    (assoc-ref project key)))

(define (unused-name-findings source used spec at warnings)
  "Return the findings about the unused names of one @code{only} clause."
  (let* ((names (spec-name-nodes spec))
         (unused (remove (lambda (node) (used? used (import-name-string node)))
                         names)))
    (if (or (null? unused) (= (length unused) (length names)))
      warnings
      (fold
       (lambda (node warnings)
         (let ((span (deletion-range (cddr (data-elements spec)) node)))
           (if span
             (cons (make-lint-warning/range
                    'unused-import 'info
                    (format #f "`~a` is imported from `~a` and never used"
                            (import-name-string node) (import-label spec))
                    (node-start node) (car span) (cdr span)
                    (lambda (text from to) ""))
                   warnings)
             warnings)))
       warnings
       unused))))

(define (unused-rename-findings source used spec warnings)
  "Return the findings about the unused pairs of one @code{rename} clause."
  (fold
   (lambda (pair warnings)
     (let* ((elements (data-elements pair))
            (fresh (and (>= (length elements) 2) (list-ref elements 1)))
            (name (and fresh (import-name-string fresh)))
            (span (deletion-range (cddr (data-elements spec)) pair)))
       (if (or (used? used name) (not span))
         warnings
         (cons (make-lint-warning/range
                'unused-import 'info
                (format #f "`~a` is imported from `~a` under a new name and never used"
                        name (import-label spec))
                (node-start pair) (car span) (cdr span)
                (lambda (text from to) ""))
               warnings))))
   warnings
   (rename-pair-nodes spec)))

(define (spec-unused? used spec ast project)
  "Return @code{#t} when nothing @var{spec} brings in is used by the file.

The two shapes that can be answered are the ones where sift can see the names:
an `only' clause whose names are all unused, and a library whose own exports
are all unused.  A `prefix', an `except' or a plain library spec from a library
sift cannot read says nothing about its names, so it is never unused as a
whole."
  (case [spec-kind spec]
    [(only)
     (let ((names (spec-name-nodes spec)))
       (and (pair? names)
         (not (any (lambda (node) (used? used (import-name-string node))) names))))]
    [(library)
     (let* ((key (library-key (spec-library spec)))
            (exports (library-exports-of key ast project)))
       (and (pair? exports)
         (not (any (lambda (name) (used? used name)) exports))))]
    [else #f]))

(define (line-start-of source index)
  "Return the index at which the line that holds @var{index} begins."
  (let loop ((at (min index (string-length source))))
    (cond
      [(<= at 0) 0]
      [(char=? (string-ref source (- at 1)) #\newline) at]
      [else (loop (- at 1))])))

(define (site-text source site)
  "Return @var{site} as it is written, on one line.

A clause is a keyword and a spec, so the text of one is the two of them: a
finding that printed `#:use-module` alone would say what was removed and not
what it named."
  (let ((form (import-site-form site))
        (anchor (import-site-anchor site)))
    (single-line
     (substring source (node-index form)
                (node-end-index (or anchor form))))))

(define (site-deletion source site)
  "Return @code{(start . end)}, what a fix deletes to remove @var{site} whole.

An `(import ...)' form goes with the lines it occupies; a `#:use-module' clause
goes with its own line, keyword included -- leaving the keyword behind would
leave `#:use-module' with nothing after it."
  (let ((form (import-site-form site))
        (anchor (import-site-anchor site)))
    (if anchor
      (cons (line-start-of source (node-index form))
            (line-start-of source (+ 1 (node-end-index anchor))))
      (cons (node-index form) (line-end-of source (node-end-index form))))))

(define (directory-of path)
  "Return the directory part of @var{path}, or @code{\".\"} when it names none.

This is Guile's @code{dirname}, written out here because it is a Guile
procedure and not an R7RS one: a path with no directory part names the current
one, and a file with no path at all names no project to look at."
  (let loop ((index (- (string-length path) 1)))
    (cond
      [(< index 0) "."]
      [(char=? (string-ref path index) #\/)
       (if (= index 0) "/" (substring path 0 index))]
      [else (loop (- index 1))])))

(define (unused-import-warnings ast source dialect)
  (let* ((used (referenced-names ast))
         (file (source-file-of ast))
         (project (project-libraries (and file (directory-of file))))
         (warnings '()))
    (for-each
     (lambda (site)
       (let* ((specs (import-site-specs site))
              (unused (filter (lambda (spec) (spec-unused? used spec ast project))
                              specs)))
         (cond
           ;; every spec unused: one finding, on the site, whose fix removes it
           [(and (pair? specs) (= (length unused) (length specs)))
            (let ((span (site-deletion source site)))
              (set! warnings
                    (cons (make-lint-warning/range
                           'unused-import 'info
                           (format #f "`~a` imports nothing this file uses"
                                   (site-text source site))
                           (node-start (import-site-form site))
                           (car span) (cdr span)
                           (lambda (text from to) ""))
                          warnings)))]
           [else
             ;; a spec of two is deleted with the space before it
             (for-each
              (lambda (spec)
                (let ((span (deletion-range specs spec)))
                  (when span
                    (set! warnings
                          (cons (make-lint-warning/range
                                 'unused-import 'info
                                 (format #f "`~a` imports nothing this file uses"
                                         (source-text source spec))
                                 (node-start spec) (car span) (cdr span)
                                 (lambda (text from to) ""))
                                warnings)))))
              unused)
             (for-each
              (lambda (spec)
                (case [spec-kind spec]
                  [(only)
                   (set! warnings (unused-name-findings source used spec 0 warnings))]
                  [(rename)
                   (set! warnings (unused-rename-findings source used spec warnings))]
                  [else #f]))
              specs)])))
     (import-sites ast))
    (reverse warnings)))

(define unused-import-checker
  (checker 'unused-import
           "A name or a library an `import` brings in that the file never uses"
           '(scheme)
           unused-import-warnings
           #:severity 'info
           #:example '((input . "(import (only (srfi 1) fold car))
(display (fold + 0 '(1 2)))")
                       (fix . "delete `car` from the `only` clause"))))

;;; ===== library-not-found =====

(define (library-not-found-warnings ast source dialect)
  (let* ((declared (declared-libraries ast))
         (file (source-file-of ast))
         (project (project-libraries (and file (directory-of file))))
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

;;; ===== unused-definition =====
;;;
;;; A module that lists what it exports has said what its public surface is.
;;; Another module cannot name anything that is not on that list, so a top-level
;;; definition that is neither exported nor mentioned anywhere in its own file
;;; is code nothing can reach.
;;;
;;; Both halves are required, and each of them alone would be a guess.  Without
;;; an `#:export' clause there is no public surface to compare against -- a
;;; script exports nothing and is right to -- so the rule says nothing.  And
;;; without counting the whole file, a name that appears under a quote, or in a
;;; form the reader kept as data, would look dead when it is not: `mentions'
;;; counts every appearance, which is why this rule misses an odd case rather
;;; than reporting one.

(define (module-level-definitions ast)
  "Return the top-level `define' and `define*' forms of @var{ast}."
  (filter (lambda (node)
            (and (node-list? node)
              (memq (node-head-name node) '(define define*))))
          (filter node-data? ast)))

(define (unused-definition-warnings ast source dialect)
  (let ((module (file-module ast)))
    (if (not module)
      '()
      (let ((exports (module-export-clause-names module)))
        (if (not exports)
          '()
          (let ((mentions (mentions ast))
                (warnings '()))
            (for-each
             (lambda (form)
               (let* ((name (defined-name form))
                      (text (and name (node-atom? name) (node-value name))))
                 (when (and text
                         (not (member text exports))
                         (= 1 (or (hash-ref mentions text) 0)))
                   (set! warnings
                         (cons (make-lint-warning
                                'unused-definition 'info
                                (format #f "`~a` is defined here, never used, and not exported; nothing can reach it"
                                        text)
                                (node-start name))
                               warnings)))))
             (module-level-definitions ast))
            (reverse warnings)))))))

(define unused-definition-checker
  (checker 'unused-definition
           "A definition nothing in the file uses, in a module that does not export it"
           '(scheme)
           unused-definition-warnings
           #:severity 'info
           #:example '((input . "(define-module (demo)\n  #:export (used))\n\n(define (used) 1)\n(define (forgotten) 2)\n")
                       (fix . "delete it, or export it if another module is meant to call it"))))

;;; ===== The list =====

(define scheme-checkers
  (list internal-define-checker
        if-without-else-checker
        letrec-single-binding-checker
        letrec-init-reference-checker
        quasiquote-unquote-constant-checker
        quasiquote-dotted-unquote-checker
        clause-brackets-checker
        docstring-texinfo-checker
        unused-binding-checker
        unused-import-checker
        library-not-found-checker
        unused-definition-checker))
