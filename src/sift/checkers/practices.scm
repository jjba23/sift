;; sift --- practices
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Every rule here is about a form that reads as if it were safe and is not
;; quite.  A quoted number is a puzzle before it is a bug -- the quote does
;; nothing, so why is it there?  `eq?' on a number or a string compares
;; identity where the author meant value, and two equal-looking literals are
;; not the same object.  A `cond' clause after `else' is code nobody can reach,
;; and a `case' datum listed in two clauses is a branch that never runs.  A
;; parameter list longer than a handful is a call nobody writes without
;; looking it up, and a `defcustom' without `:type' is a setting Customize
;; cannot render.
;;
;; The second group is about a form that says the same thing twice or not quite
;; what it means: an `apply' that builds the list it is about to spread, a
;; `let' whose initializer reaches for a name the same `let' is still binding,
;; a `let*' with nothing to sequence, an `append' of the empty list, and a
;; `case' clause keyed on a string, which `eqv?' cannot match.
;;
;; All but the first report and stop there.  Which comparison an author meant,
;; and what to name the sixth parameter, are decisions; the quote is not, which
;; is why it is the one rule here with a fix.

(define-module (sift checkers practices)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift check base)
  #:export (practices-checkers
            quote-of-constant-checker
            eq-on-literal-checker
            cond-else-not-last-checker
            duplicate-case-datum-checker
            too-many-arguments-checker
            defcustom-without-type-checker
            apply-of-list-checker
            let-sibling-checker
            needless-let*-checker
            eq-nil-checker
            append-nothing-checker
            case-literal-datum-checker))

;;; ===== Reading =====

(define (node-text source node)
  "Return the source text of @var{node}."
  (substring source (node-index node) (node-end-index node)))

(define (literal-atom? node)
  "Return @code{#t} when @var{node} is an atom that evaluates to itself."
  (and (node-atom? node)
    (memq (node-atom-type node) '(number string character boolean keyword))))

(define (clause-head clause)
  "Return the operator a @code{cond} or @code{case} clause starts with."
  (and (node-list? clause) (node-head-name clause)))

;;; ===== quote-of-constant =====
;;;
;;; The reader turns `'x' into `(quote x)', and the span of that form covers
;;; the quote character, so the fix is the datum's own text in the form's own
;;; range.  `'()' is not here: the empty list is the one datum a quote is
;;; still the only way to write.

(define (quoted-datum node)
  "Return the datum @var{node} quotes, or @code{#f}.

The reader keeps an apostrophe as a prefix node around the datum, and `(quote x)'
written out is a list, so both shapes are here.  The prefix's span covers the
quote character, which is what makes the fix a replacement of the whole node."
  (cond
    [(and (node-prefix? node) (equal? (node-token node) "'"))
     (node-child node)]
    [(list-headed-by? node 'quote)
     (let ((arguments (form-arguments node)))
       (and (pair? arguments) (null? (cdr arguments)) (car arguments)))]
    [else #f]))

(define (quote-of-constant-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
     (lambda (node)
       (let ((datum (quoted-datum node)))
         (when (and datum (literal-atom? datum))
           (set! warnings
                 (cons (make-lint-warning/range
                        'quote-of-constant 'info
                        "a self-evaluating datum does not need a quote"
                        (node-start node)
                        (node-index node) (node-end-index node)
                        (lambda (text start end) (node-text text datum)))
                       warnings)))))
     ast)
    (reverse warnings)))

;;; ===== eq-on-literal =====
;;;
;;; `eq?' asks whether two objects are the same one, and a number or a string
;;; literal is not promised to be: an implementation may copy either.  The
;;; advice names the comparison the datum wants rather than rewriting the call,
;;; because `eqv?' and `equal?' mean different things one level down.

(define (literal-comparison node)
  (case [node-atom-type node]
    [(number character) "`eqv?`"]
    [(string) "`equal?`"]
    [else #f]))

(define (eq-on-literal-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
     (lambda (node)
       (when (head-is-one-of? node '(eq? eq))
         (for-each
          (lambda (literal)
            (let ((advice (literal-comparison literal)))
              (when advice
                (set! warnings
                      (cons (make-lint-warning
                             'eq-on-literal 'warning
                             (format #f "`~a` compares identity; a ~a literal wants ~a"
                                     (node-head-name node)
                                     (node-atom-type literal)
                                     advice)
                             (node-start literal))
                            warnings)))))
          (form-arguments node))))
     ast)
    (reverse warnings)))

;;; ===== cond-else-not-last =====

(define (cond-else-not-last-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
     (lambda (node)
       (when (list-headed-by? node 'cond)
         (let* ((clauses (form-arguments node))
                (last (and (pair? clauses) (car (reverse clauses)))))
           (for-each
            (lambda (clause)
              (when (and (not (eq? clause last))
                      (memq (clause-head clause) '(else t)))
                (set! warnings
                      (cons (make-lint-warning
                             'cond-else-not-last 'warning
                             "a clause that always matches makes every clause after it unreachable"
                             (node-start clause))
                            warnings))))
            clauses))))
     ast)
    (reverse warnings)))

;;; ===== duplicate-case-datum =====
;;;
;;; A `case' clause is `((datum ...) body ...)' or `(datum body ...)', and the
;;; default is `else' (or `otherwise', or `t').  The datum is compared as
;;; written, which is what `case' itself does.

(define (case-default-head? head)
  (memq head '(else otherwise t)))

(define (case-datum-nodes clause)
  "Return the datum nodes a @code{case} clause matches, and none for a default."
  (let* ((elements (and (node-list? clause) (form-elements clause)))
         (first (and (pair? elements) (car elements))))
    (cond
      [(not first) '()]
      [(node-atom? first)
       (if (case-default-head? (atom-symbol first)) '() (list first))]
      [(node-list? first) (form-elements first)]
      [else '()])))

(define (duplicate-case-datum-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
     (lambda (node)
       (when (head-is-one-of? node '(case ecase ccase))
         (let ((seen '()))
           (for-each
            (lambda (clause)
              (for-each
               (lambda (datum)
                 (let ((text (node-text source datum)))
                   (if (member text seen)
                     (set! warnings
                           (cons (make-lint-warning
                                  'duplicate-case-datum 'warning
                                  (format #f "`~a` is matched by an earlier clause, so this one cannot run"
                                          text)
                                  (node-start datum))
                                 warnings))
                     (set! seen (cons text seen)))))
               (case-datum-nodes clause)))
            (form-arguments node)))))
     ast)
    (reverse warnings)))

;;; ===== apply-of-list =====
;;;
;;; `(apply f a (list b c))' builds a list in order to spread it again, and
;;; `(apply f '())' spreads nothing at all.  Both are a call the reader can
;;; write directly, and the fix is that call.

(define (empty-list-datum? node)
  "Return @code{#t} when @var{node} is the empty list, written with a quote.

`()' is not an expression in Scheme, so the empty list only ever appears
under a quote -- `'()' or `(quote ())', which are the two shapes this reads."
  (let ((datum (quoted-datum node)))
    (and datum (node-list? datum) (null? (node-elements datum)))))

(define (apply-of-list-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (when (and (list-headed-by? node 'apply)
               (>= (argument-count node) 2))
         (let* ((arguments (form-arguments node))
                (spread (car (reverse arguments)))
                (prefix (map (lambda (argument) (node-text source argument))
                             (reverse (cdr (reverse arguments))))))
           (define (fix parts)
             (format #f "(~a)" (string-join parts " ")))
           (cond
             [(list-headed-by? spread 'list)
              (set! warnings
                    (cons (make-lint-warning/range
                           'apply-of-list 'info
                           "`apply` of a `list` is a call; write the arguments where they are read"
                           (node-start node)
                           (node-index node) (node-end-index node)
                           (lambda (text start end)
                             (fix (append prefix
                                          (map (lambda (element)
                                                 (node-text source element))
                                               (form-arguments spread))))))
                          warnings))]
             [(empty-list-datum? spread)
              (set! warnings
                    (cons (make-lint-warning/range
                           'apply-of-list 'info
                           "`apply` with an empty list is a call with no arguments"
                           (node-start node)
                           (node-index node) (node-end-index node)
                           (lambda (text start end) (fix prefix)))
                          warnings))]))))
     ast)
    (reverse warnings)))

;;; ===== let-sibling =====
;;;
;;; `(let ((a (f)) (b (g a))) ...)' reads as if `b' could see `a', and it
;;; cannot: the initializers of a `let' are evaluated in the enclosing scope,
;;; where `a' may not exist at all.  `let*' is the form that binds in order.
;;;
;;; A mention only counts where it is evaluated and outside a scope that binds
;;; the name again, so `'(a)' and `(let ((a 2)) a)' inside an initializer are
;;; not references to the binding being checked.

(define (symbol-name node)
  "Return the name of @var{node} when it is one plain symbol, else @code{#f}."
  (and (node-atom? node) (eq? (node-atom-type node) 'symbol) (node-value node)))

(define (binding-elements node)
  "Return the binding list of a `let`-like form @var{node}, or @code{#f}."
  (let ((arguments (form-arguments node)))
    (and (pair? arguments) (node-list? (car arguments))
      (node-elements (car arguments)))))

(define (binding-name binding)
  "Return the name a binding binds, or @code{#f}."
  (let ((parts (and (node-list? binding) (node-elements binding))))
    (and (pair? parts) (symbol-name (car parts)))))

(define (binding-scope? name ancestors)
  "Return @code{#t} when an enclosing form binds @var{name} again."
  (any (lambda (ancestor)
         (cond
           [(formals-holder? ancestor)
            (let ((formals (formals-of-form ancestor)))
              (and formals
                (any (lambda (element) (equal? name (symbol-name element)))
                     (form-elements formals))))]
           [(and (node-list? ancestor)
              (memq (node-head-name ancestor)
                    '(let let* letrec letrec* let-values let*-values do)))
            (member name
                    (filter-map binding-name
                                (or (binding-elements ancestor) '())))]
           [else #f]))
       ancestors))

(define (references? node names)
  "Return @code{#t} when @var{node} evaluates one of @var{names} somewhere."
  (let ((found #f))
    (walk-with-ancestors
     (lambda (candidate ancestors)
       (let ((name (symbol-name candidate)))
         (when (and name (member name names)
                 (code-position? candidate ancestors)
                 (not (binding-scope? name ancestors)))
           (set! found #t))))
     (list node))
    found))

(define (binding-value binding)
  "Return the initializer of @var{binding}, or @code{#f}."
  (let ((parts (form-elements binding)))
    (and (pair? parts) (pair? (cdr parts)) (cadr parts))))

(define (let-sibling-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (when (list-headed-by? node 'let)
         (let* ((bindings (or (binding-elements node) '()))
                (names (filter-map binding-name bindings)))
           (for-each
            (lambda (binding)
              (let ((value (binding-value binding)))
                (when (and value (references? value names))
                  (set! warnings
                        (cons (make-lint-warning/range
                               'let-sibling 'warning
                               "an initializer refers to a name this `let` is still binding; `let*` binds in order"
                               (node-start value)
                               (node-index (node-head node))
                               (node-end-index (node-head node))
                               (lambda (text start end) "let*"))
                              warnings)))))
            bindings))))
     ast)
    (reverse warnings)))

;;; ===== needless-let* =====
;;;
;;; The other half of the same choice: a `let*' no initializer depends on is a
;;; `let`, and the `let` is the one that says the order does not matter.  One
;;; binding is `let-simplification`'s, so this asks about two or more.

(define (needless-let*-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (when (and (list-headed-by? node 'let*)
               (>= (length (or (binding-elements node) '())) 2))
         (let ((bindings (binding-elements node)))
           (let loop ((rest bindings) (earlier '()) (dependent #f))
             (cond
               [(null? rest)
                (unless dependent
                  (set! warnings
                        (cons (make-lint-warning/range
                               'needless-let* 'info
                               "no initializer refers to an earlier binding, so `let` says the same"
                               (node-start (node-head node))
                               (node-index (node-head node))
                               (node-end-index (node-head node))
                               (lambda (text start end) "let"))
                              warnings)))]
               [else
                 (let ((value (binding-value (car rest))))
                   (loop (cdr rest)
                         (cons (binding-name (car rest)) earlier)
                         (or dependent (and value (references? value earlier)))))])))))
     ast)
    (reverse warnings)))

;;; ===== eq-nil =====
;;;
;;; `(eq? x '())' asks whether two objects are the same one, and the empty
;;; list is not promised to be a single object.  What the author meant is
;;; whether x is empty, which `null?' answers -- and which is the only test
;;; that is right for every value, since `nil' is the whole of falsehood in
;;; Emacs Lisp and Common Lisp.

(define (nil-datum? node)
  "Return @code{#t} when @var{node} is `nil' or a quoted empty list."
  (or (and (node-atom? node) (eq? (node-atom-type node) 'symbol)
        (string=? (node-value node) "nil"))
    (empty-list-datum? node)))

(define (eq-nil-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (when (and (head-is-one-of? node '(eq? eq equal? equal))
               (= 2 (argument-count node)))
         (let ((arguments (form-arguments node)))
           (when (= 1 (length (filter nil-datum? arguments)))
             (let* ((other (find (lambda (argument) (not (nil-datum? argument)))
                                 arguments))
                    (procedure (if (memq dialect '(cl elisp)) "null" "null?")))
               (set! warnings
                     (cons (make-lint-warning/range
                            'eq-nil 'warning
                            (format #f "a comparison with the empty list asks whether the value is empty; `~a` says so"
                                    procedure)
                            (node-start node)
                            (node-index node) (node-end-index node)
                            (lambda (text start end)
                              (format #f "(~a ~a)" procedure
                                      (node-text source other))))
                           warnings)))))))
     ast)
    (reverse warnings)))

;;; ===== append-nothing =====
;;;
;;; The empty list is the identity of `append', so an argument that is one
;;; adds nothing to the result -- only the copy `append' was already making.

(define (append-nothing-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (when (list-headed-by? node 'append)
         (let ((arguments (form-arguments node)))
           (when (any empty-list-datum? arguments)
             (let ((kept (remove empty-list-datum? arguments)))
               (set! warnings
                     (cons (make-lint-warning/range
                            'append-nothing 'info
                            "an `append` of the empty list is the other list"
                            (node-start node)
                            (node-index node) (node-end-index node)
                            (lambda (text start end)
                              (cond
                                [(null? kept) "'()"]
                                [(null? (cdr kept)) (node-text source (car kept))]
                                [else
                                  (format #f "(append ~a)"
                                          (string-join
                                           (map (lambda (argument)
                                                  (node-text source argument))
                                                kept)
                                           " "))])))
                           warnings)))))))
     ast)
    (reverse warnings)))

;;; ===== case-literal-datum =====
;;;
;;; A `case' clause is compared with `eqv?', which decides numbers, characters
;;; and symbols and nothing else.  A string datum is a clause that cannot
;;; match however equal the two strings are, which is why Guile's own compiler
;;; warns about it and this rule does too.

(define (case-literal-datum-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (when (head-is-one-of? node '(case ecase ccase))
         (for-each
          (lambda (clause)
            (for-each
             (lambda (datum)
               (when (or (string-atom? datum) (node-vector? datum))
                 (set! warnings
                       (cons (make-lint-warning
                              'case-literal-datum 'warning
                              "`case` compares its datum with `eqv?`, which cannot decide a string; use `cond` with `string=?`"
                              (node-start datum))
                             warnings))))
             (case-datum-nodes clause)))
          (form-arguments node))))
     ast)
    (reverse warnings)))

;;; ===== too-many-arguments =====

(define (too-many-arguments-warnings ast source dialect)
  (let ((limit (or (option 'max-arguments 5)))
        (warnings '()))
    (for-each-node
     (lambda (node)
       ;; a definition has parameters, and only the ones a caller must pass
       ;; positionally are the ones a call site has to read past
       (let ((formals (and (definition-form? node) (formal-elements node))))
         (when formals
           (let ((count (required-positional-formals formals)))
             (when (> count limit)
               (set! warnings
                     (cons (make-lint-warning
                            'too-many-arguments 'info
                            (format #f "this definition takes ~a positional arguments; beyond ~a the call site stops reading as a sentence"
                                    count limit)
                            (node-start (or (defined-name node) node)))
                           warnings)))))))
     ast)
    (reverse warnings)))

;;; ===== defcustom-without-type =====

(define (plist-has-key? source nodes key)
  "Return @code{#t} when @var{nodes} holds @var{key} at an odd position.

The key is compared as written, because a keyword atom's symbol has already
lost its colon."
  (let loop ((rest nodes))
    (cond
      [(or (null? rest) (null? (cdr rest))) #f]
      [(and (node-atom? (car rest))
         (string=? (node-text source (car rest)) key)) #t]
      [else (loop (cddr rest))])))

(define (defcustom-without-type-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
     (lambda (node)
       (when (list-headed-by? node 'defcustom)
         (let* ((arguments (form-arguments node))
                (name (and (pair? arguments) (car arguments)))
                (options (if (>= (length arguments) 3)
                           (cdddr arguments)
                           '())))
           (unless (plist-has-key? source options ":type")
             (set! warnings
                   (cons (make-lint-warning
                          'defcustom-without-type 'warning
                          "a `defcustom` without `:type` is a setting Customize cannot edit"
                          (and name (node-start name)))
                         warnings))))))
     ast)
    (reverse warnings)))

;;; ===== The checkers =====

(define quote-of-constant-checker
  (checker 'quote-of-constant
           "A quote around a datum that already evaluates to itself"
           '(scheme elisp cl)
           quote-of-constant-warnings
           #:severity 'info
           #:example '((input . "'5")
                       (fix . "5"))))

(define eq-on-literal-checker
  (checker 'eq-on-literal
           "`eq?` asked about a number or a string, where identity is not the question"
           '(scheme elisp cl)
           eq-on-literal-warnings
           #:severity 'warning
           #:example '((input . "(eq? (length items) 3)")
                       (fix . "(eqv? (length items) 3)"))))

(define cond-else-not-last-checker
  (checker 'cond-else-not-last
           "A `cond` clause after `else`, which can never run"
           '(scheme elisp cl)
           cond-else-not-last-warnings
           #:severity 'warning
           #:example '((input . "(cond ((a) 1) (else 2) ((b) 3))")
                       (fix . "(cond ((a) 1) ((b) 3) (else 2))"))))

(define duplicate-case-datum-checker
  (checker 'duplicate-case-datum
           "A `case` datum matched by an earlier clause"
           '(scheme elisp cl)
           duplicate-case-datum-warnings
           #:severity 'warning
           #:example '((input . "(case x ((a) 1) ((a b) 2))")
                       (fix . "(case x ((a) 1) ((b) 2))"))))

(define too-many-arguments-checker
  (checker 'too-many-arguments
           "A parameter list long enough that a call site has to be looked up"
           '(scheme elisp cl)
           too-many-arguments-warnings
           #:severity 'info
           #:example '((input . "(define (place x y width height colour outline) ...)")
                       (fix . "(define (place x y area colour) ...)"))))

(define defcustom-without-type-checker
  (checker 'defcustom-without-type
           "A `defcustom` whose value Customize cannot render for lack of `:type`"
           '(elisp)
           defcustom-without-type-warnings
           #:severity 'warning
           #:example '((input . "(defcustom sift-thing nil :group 'sift)")
                       (fix . "(defcustom sift-thing nil :type 'boolean :group 'sift)"))))

;;; ===== The checkers =====

(define apply-of-list-checker
  (checker 'apply-of-list
           "`apply` of a `list`, or of the empty list, where a call says it once"
           '(scheme elisp cl)
           apply-of-list-warnings
           #:severity 'info
           #:example '((input . "(apply f 1 (list 2 3))")
                       (fix . "(f 1 2 3)"))))

(define let-sibling-checker
  (checker 'let-sibling
           "A `let` initializer that refers to a name the same `let` binds"
           '(scheme elisp cl)
           let-sibling-warnings
           #:severity 'warning
           #:example '((input . "(let ((a (f)) (b (g a))) b)")
                       (fix . "(let* ((a (f)) (b (g a))) b)"))))

(define needless-let*-checker
  (checker 'needless-let*
           "A `let*` none of whose initializers depends on an earlier binding"
           '(scheme elisp cl)
           needless-let*-warnings
           #:severity 'info
           #:example '((input . "(let* ((a (f)) (b (g))) (+ a b))")
                       (fix . "(let ((a (f)) (b (g))) (+ a b))"))))

(define eq-nil-checker
  (checker 'eq-nil
           "`eq?` or `equal?` against `nil`, where `null` asks the question"
           '(scheme elisp cl)
           eq-nil-warnings
           #:severity 'warning
           #:example '((input . "(eq? x '())")
                       (fix . "(null? x)"))))

(define append-nothing-checker
  (checker 'append-nothing
           "An `append` with the empty list among its arguments"
           '(scheme elisp cl)
           append-nothing-warnings
           #:severity 'info
           #:example '((input . "(append xs '())")
                       (fix . "xs"))))

(define case-literal-datum-checker
  (checker 'case-literal-datum
           "A `case` datum that `eqv?` cannot decide, such as a string"
           '(scheme cl)
           case-literal-datum-warnings
           #:severity 'warning
           #:example '((input . "(case x ((\"a\") 1))")
                       (fix . "(cond ((string=? x \"a\") 1))"))))

;;; ===== The list =====

(define practices-checkers
  (list quote-of-constant-checker
        eq-on-literal-checker
        cond-else-not-last-checker
        duplicate-case-datum-checker
        too-many-arguments-checker
        defcustom-without-type-checker
        apply-of-list-checker
        let-sibling-checker
        needless-let*-checker
        eq-nil-checker
        append-nothing-checker
        case-literal-datum-checker))
