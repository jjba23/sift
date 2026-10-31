;; sift --- ergonomics checkers
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Two suggestions about the *shape* of a call rather than about its
;; correctness.  Lisp has two ways of keeping a form readable: bind the part
;; that got long, and name the arguments that stopped fitting.  These
;; checkers say when one of those moments has arrived; neither has an
;; automatic fix, because choosing the name and the split is the author's
;; work.

(define-module (sift checkers ergonomics)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift format)
  #:use-module (sift check base)
  #:export (ergonomics-checkers
              wrap-in-let-checker
              keyword-arguments-checker))

;;; ===== wrap-in-let =====

(define (form-depth node)
  "Return how deeply @var{node} nests lists inside itself.

An atom has depth 0, a call whose arguments are atoms depth 1, and a call
with a call inside it depth 2.  This is the number that grows when a body
form is written as one long expression instead of a few named steps."
  (if (node-list? node)
    (+ 1 (apply max 0 (map form-depth (node-elements node))))
    0))

(define (body-owner-heads)
  '(define define* define-public lambda let let* letrec letrec*
     let-values let*-values parameterize
     defun defmacro defsubst cl-defun cl-defmacro
     defmethod defgeneric))

(define (body-skip head)
  (case head
    [(define define* define-public) 2]
    [(lambda lambda*) 1]
    [(let let* letrec letrec* let-values let*-values parameterize) 2]
    [(defun defmacro defsubst cl-defun cl-defmacro defmethod defgeneric) 3]
    [else 2]))

(define (wrap-in-let-warnings ast source dialect)
  (let ((max-depth (or (option 'max-depth 6)))
        (max-elements (or (option 'max-elements 20)))
        (warnings '()))
    (define (report node message)
      (set! warnings
        (cons (make-lint-warning 'wrap-in-let 'info message (node-start node))
          warnings)))
    (for-each-form
       (lambda (node)
         (let ((head (and (node-list? node) (node-head-name node))))
           (when (and head (memq head (body-owner-heads)))
             (for-each
                (lambda (form)
                  (when (node-list? form)
                    (let ((depth (form-depth form))
                          (elements (length (node-elements form))))
                      (cond
                        [(> depth max-depth)
                         (report form
                           (format #f "this body form nests ~a deep; bind part of it"
                             depth))]
                        [(> elements max-elements)
                         (report form
                           (format #f "this body form has ~a elements; bind part of it in a `let`"
                             elements))]))))
                (body-elements node (body-skip head))))))
       ast)
    (reverse warnings)))

(define wrap-in-let-checker
  (checker 'wrap-in-let
    "A body form that is too deep or too long to read as one expression"
    'all
    wrap-in-let-warnings
    #:severity 'info
    #:example '((input . "(define (f x)\n  (list (car (cdr (assq 'a (cdr x))))))")
                (fix . "no automatic fix; bind the repeated part in a `let`"))))

;;; ===== unused-argument =====

(define definition-heads
  '(define define* define-public lambda lambda* defun defmacro defsubst
     cl-defun cl-defmacro cl-defsubst defmethod defgeneric))

(define (formals-of node)
  "Return the parameter nodes of a definition @var{node}, or @code{#f}.

For `(define (f x y) ...)' the parameters are the elements of the head list
after the name; for `(defun f (x y) ...)' they are the elements of the second
argument."
  (let* ((head (node-head-name node))
         (elements (node-elements node))
         (second (and (pair? elements) (pair? (cdr elements)) (cadr elements)))
         (third (and (pair? elements) (pair? (cdr elements))
                  (pair? (cddr elements)) (car (cddr elements)))))
    (cond
      [(not head) #f]
      [(memq head '(lambda lambda*))
       (and (node-list? second) (node-elements second))]
      [(memq head '(define define* define-public))
       (and (node-list? second)
         (let ((rest (node-elements second)))
           (and (pair? rest) (cdr rest))))]
      [(memq head '(defun defmacro defsubst cl-defun cl-defmacro cl-defsubst
                     defmethod defgeneric))
       ;; the name comes first, the parameter list second
       (and (node-list? third) (node-elements third))]
      [else #f])))

(define (formal-names formals)
  "Return the parameter names in @var{formals}, as @code{(node . text)} pairs.

Markers (`&optional', `#:key') and destructuring lists are not parameters of
their own; a parameter written `(name default)' contributes `name'."
  (append-map
     (lambda (element)
       (cond
         [(and (node-atom? element) (eq? (node-atom-type element) 'symbol))
          (let ((text (node-value element)))
            (if (or (string-prefix? "&" text) (string-prefix? "#:" text))
              '()
              (list (cons element text))))]
         [(node-list? element)
          ;; a parameter with a default, `(name value)'
          (let ((first (node-head element)))
            (if (and first (node-atom? first)
                  (eq? (node-atom-type first) 'symbol))
              (list (cons first (node-value first)))
              (formal-names (node-elements element))))]
         [else '()]))
     formals))

(define (name-uses node name)
  "Return how many times the symbol @var{name} appears under @var{node}."
  (fold-nodes
     (lambda (candidate count)
       (if (and (node-atom? candidate)
             (eq? (node-atom-type candidate) 'symbol)
             (string=? (node-value candidate) name))
         (+ count 1)
         count))
     0
     (list node)))

(define (ignored-parameter? name)
  (or (string-prefix? "_" name)
    (string=? name "-")
    (string=? name "ignored")))

(define (unused-argument-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
       (lambda (node)
         (let ((formals (and (memq (node-head-name node) definition-heads)
                          (formals-of node))))
           (when formals
             (for-each
                (lambda (parameter)
                  (let ((name (cdr parameter)))
                    (unless (ignored-parameter? name)
                      ;; the binding counts once; anything else is a use, and
                      ;; `_' in front of the name is how a program says it is
                      ;; taking an argument it does not need
                      (when (< (name-uses node name) 2)
                        (set! warnings
                          (cons (make-lint-warning 'unused-argument 'info
                                  (format #f "the argument `~a` is never used; name it `_~a`"
                                    name name)
                                  (node-start (car parameter)))
                            warnings))))))
                (formal-names formals)))))
       ast)
    (reverse warnings)))

(define unused-argument-checker
  (checker 'unused-argument
    "A parameter that the definition never refers to"
    'all
    unused-argument-warnings
    #:severity 'info
    #:example '((input . "(define (f x y) (list x))")
                (fix . "no automatic fix; use it, drop it, or name it `_y`"))))

;;; ===== nesting-depth =====

;; A Lisp nests: a `let' inside a `define' inside a `cond' clause is ordinary
;; code.  What is *not* ordinary is a decision inside a decision inside a
;; decision -- the form a reader has to hold four alternatives of at once.
;; So the depth that counts is the number of enclosing *conditional* forms,
;; not the number of enclosing parentheses.
(define conditional-heads
  '(if cond case when unless and or do loop while match pcase cond-expand
     if-let when-let if-let* when-let* typecase etypecase ctypecase ecase
     ccase handler-case restart-case))

(define (conditional-depth node)
  "Return the deepest chain of conditionals inside @var{node}."
  (if (node-list? node)
    (let ((inside (apply max 0 (map conditional-depth (node-elements node)))))
      (if (memq (node-head-name node) conditional-heads)
        (+ 1 inside)
        inside))
    0))

(define (definition-body-depth node)
  "Return the deepest chain of conditionals in the body of @var{node}."
  (let ((head (node-head-name node)))
    (case head
      [(lambda lambda*) (apply max 0 (map conditional-depth (body-elements node 1)))]
      [(define define* define-public)
       (apply max 0 (map conditional-depth (body-elements node 2)))]
      [(defun defmacro defsubst cl-defun cl-defmacro cl-defsubst
         defmethod defgeneric)
       (apply max 0 (map conditional-depth (body-elements node 3)))]
      [else 0])))

(define (nesting-depth-warnings ast source dialect)
  (let ((limit (or (option 'max-definition-depth 4)))
        (warnings '()))
    (for-each-form
       (lambda (node)
         (when (memq (node-head-name node) definition-heads)
           (let ((depth (definition-body-depth node)))
             (when (> depth limit)
               (set! warnings
                 (cons (make-lint-warning 'nesting-depth 'warning
                         (format #f "this definition chains ~a conditionals deep; split it"
                           depth)
                         (node-start node))
                   warnings))))))
       ast)
    (reverse warnings)))

(define nesting-depth-checker
  (checker 'nesting-depth
    "A definition whose body nests deeper than the configured limit"
    'all
    nesting-depth-warnings
    #:severity 'warning
    #:example '((input . "(define (f x)\n  (if (a) (if (b) (if (c)\n    (if (d) (if (e) 1 2))))))")
                (fix . "no automatic fix; extract the inner form into its own procedure"))))

;;; ===== keyword-arguments =====

;; Calls that are meant to take as many arguments as you like.  Reporting
;; them would be noise, and a project can add its own by excluding the
;; checker.
(define variadic-operators
  '(list vector list* append apply call-with-values values
     format error warn display string-append string-join
     + * and or begin cond case))

(define (keyword-arguments-warnings ast source dialect)
  (let ((max-positional (or (option 'max-positional 5)))
        (warnings '()))
    (for-each-form
       (lambda (node)
         (when (node-list? node)
           (let* ((name (node-head-name node))
                  (arguments (form-arguments node)))
             (when (and name
                     (not (memq name variadic-operators))
                     ;; a form in the indentation table is a special form or a
                     ;; known macro, and knows its own argument list
                     (not (indent-rule dialect name))
                     (> (length arguments) max-positional)
                     (not (any keyword-node? arguments)))
               (set! warnings
                 (cons (make-lint-warning 'keyword-arguments 'info
                         (format #f "`~a` takes ~a positional arguments; consider `:key`"
                           name (length arguments))
                         (node-start node))
                   warnings))))))
       ast)
    (reverse warnings)))

(define (keyword-node? node)
  "Return @code{#t} when @var{node} is a keyword such as @code{:key}."
  (and (node-atom? node) (eq? (node-atom-type node) 'keyword)))

(define keyword-arguments-checker
  (checker 'keyword-arguments
    "A call with more positional arguments than a reader can keep in mind"
    'all
    keyword-arguments-warnings
    #:severity 'info
    #:example '((input . "(make-window a b c d e f g)")
                (fix . "no automatic fix; name the arguments and pass them as keywords"))))

;;; ===== The list =====

(define ergonomics-checkers
  (list wrap-in-let-checker
    keyword-arguments-checker
    unused-argument-checker
    nesting-depth-checker))
