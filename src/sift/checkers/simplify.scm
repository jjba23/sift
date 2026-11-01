;; sift --- simplifications
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A Lisp rewards writing the same meaning in fewer parentheses, and the
;; shapes below are the ones where the shorter form is not a matter of taste:
;; `(lambda (x) (f x))' *is* `f', `((lambda (x) body) arg)' *is* a `let', and
;; `(let ((x e)) x)' *is* `e'.  The rules say so, and each of them has a fix
;; that writes the short form.
;;
;; What none of them does is guess.  Every condition that would make the
;; rewrite wrong is checked before the warning is reported: the formals are one
;; plain symbol, the call passes exactly that symbol and nothing else, the
;; operator is a procedure rather than a special form or a macro, the binding
;; is not shadowed.  A simplification rule that is wrong once is a rule the
;; author turns off and never turns back on.
;;
;; The readers of the three dialects are the same here, the writers are not:
;; `(lambda (x) (f x))' is a function in Scheme and in Emacs Lisp but not in
;; Common Lisp, where a function is written `#'f' and a bare lambda is not
;; even read; and `compose' is Guile's, so it is suggested in Scheme only.

(define-module (sift checkers simplify)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift check base)
  #:export (simplify-checkers
            eta-reduction-checker
            beta-reduction-checker
            composition-checker
            let-simplification-checker))

;;; ===== What a rewrite is allowed to touch =====

;;; The operators that are not procedures: a lambda whose body calls one of
;;; these is not a function, and rewriting it to the operator would be
;;; rewriting it to a keyword.
(define special-forms
  '(if when unless cond case case-lambda and or not
     let let* letrec letrec* let-values let*-values let-syntax letrec-syntax
     lambda define define* define-public define-syntax define-values
     begin do delay delay-force force make-promise parameterize guard
     quote quasiquote unquote unquote-splicing set! syntax-rules syntax-case
     named-lambda lambda*
     defun defmacro defsubst defvar defconst defcustom defparameter
     defclass defmethod defgeneric defstruct declare function
     let-over-lambda the the-future future))

(define (plain-symbol node)
  "Return the name of @var{node} when it is one plain symbol, else @code{#f}."
  (and (node-atom? node)
    (eq? (node-atom-type node) 'symbol)
    (let ((text (node-value node)))
      (and (not (string-null? text))
        (not (char=? (string-ref text 0) #\&))
        text))))

(define (formals-of node)
  "Return the single formal of a lambda @var{node}, or @code{#f}.

A lambda whose formals are anything other than one plain symbol -- a rest
argument, keywords, optionals, a dotted list -- is not a shape these rules
can rewrite without changing what it accepts."
  (let* ((arguments (form-arguments node))
         (formals (and (pair? arguments) (car arguments))))
    (and (node-list? formals)
      (let ((elements (node-elements formals)))
        (and (= 1 (length elements)) (plain-symbol (car elements)))))))

(define (lambda-body node)
  "Return the body forms of a lambda @var{node}, or @code{'()}."
  (let ((arguments (form-arguments node)))
    (if (and (pair? arguments) (pair? (cdr arguments)))
      (cdr arguments)
      '())))

(define (single-body node)
  "Return the one body form of @var{node}, or @code{#f}."
  (let ((body (lambda-body node)))
    (and (= 1 (length body)) (car body))))

(define (call-head-name node)
  "Return the operator of @var{node} as a symbol, or @code{#f}."
  (and (node-list? node) (node-head-name node)))

(define (rewritable-operator? node)
  "Return @code{#t} when @var{node} can stand for a procedure.

A call whose operator is an atom that is not a special form and not a macro
sift knows by name is a call to a procedure, and a procedure is a value."
  (let ((name (call-head-name node)))
    (and name (not (memq name special-forms)))))

(define (report warnings name severity message node fix-text)
  "Add a warning about @var{node}, with a fix that replaces it by @var{fix-text}."
  (cons (make-lint-warning/range name severity message
                                 (node-start node)
                                 (node-index node) (node-end-index node)
                                 (lambda (source start end) fix-text))
        warnings))

;;; ===== eta-reduction =====
;;;
;;; `(lambda (x) (f x))' is `f' -- except that `f' is looked up when the
;;; lambda is called and `(f x)' is looked up when it is called too, so the two
;;; differ only if `f' is redefined in between, which is what a formatter
;;; assumes away.

(define (eta-reduction-warnings ast source dialect)
  (let ((warnings '()))
    (when (eq? dialect 'scheme)
      (walk-forms
       (lambda (node ancestors)
         (when (eq? (call-head-name node) 'lambda)
           (let* ((formal (formals-of node))
                  (body (single-body node))
                  (called (and body (node-list? body) body))
                  (arguments (and called (form-arguments called))))
             (when (and formal
                     called
                     (rewritable-operator? called)
                     (= 1 (length arguments))
                     (equal? formal (plain-symbol (car arguments)))
                     (not (eq? (string->symbol formal) (call-head-name called))))
               (set! warnings
                     (report warnings 'eta-reduction 'info
                             (format #f "`(lambda (~a) (~a ~a))` is `~a`; write the procedure itself"
                                     formal (call-head-name called) formal
                                     (call-head-name called))
                             node
                             (node->string (node-head called))))))))
       ast))
    (reverse warnings)))

;;; ===== beta-reduction =====
;;;
;;; `((lambda (x) body ...) value)' is `(let ((x value)) body ...)': the value
;;; is evaluated once, before the body, exactly as the call evaluates it.

(define (beta-reduction-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (when (node-list? node)
         (let* ((operator (node-head node))
                (arguments (form-arguments node))
                (formal (and operator (lambda-like? operator) (formals-of operator)))
                (body (and operator (lambda-like? operator) (lambda-body operator))))
           (when (and formal body
                   (= 1 (length arguments))
                   (pair? body))
             (let ((value (car arguments)))
               (set! warnings
                     (report warnings 'beta-reduction 'info
                             (format #f "a lambda applied in place is a `let`; bind `~a` once"
                                     formal)
                             node
                             (format #f "(let ((~a ~a)) ~a)"
                                     formal
                                     (substring source (node-index value) (node-end-index value))
                                     (string-join
                                      (map (lambda (form)
                                             (substring source (node-index form)
                                                        (node-end-index form)))
                                           body)
                                      " ")))))))))
     ast)
    (reverse warnings)))

(define (lambda-like? node)
  (and (node-list? node) (eq? (node-head-name node) 'lambda)))

;;; ===== composition =====
;;;
;;; `(lambda (x) (f (g x)))' is `(compose f g)' in Guile, and the point of the
;;; rule is that the pipeline the author wrote is sometimes worth naming.

(define (composition-warnings ast source dialect)
  (let ((warnings '()))
    (when (eq? dialect 'scheme)
      (for-each-form
       (lambda (node)
         (when (and (eq? (call-head-name node) 'lambda)
                 (formals-of node)
                 (single-body node))
           (let* ((formal (formals-of node))
                  (body (single-body node))
                  (outer (and (node-list? body) body))
                  (outer-arguments (and outer (form-arguments outer)))
                  (inner (and outer-arguments
                           (= 1 (length outer-arguments))
                           (node-list? (car outer-arguments))
                           (car outer-arguments)))
                  (inner-arguments (and inner (form-arguments inner))))
             (when (and outer
                     (rewritable-operator? outer)
                     (node-atom? (node-head outer))
                     inner
                     (rewritable-operator? inner)
                     (node-atom? (node-head inner))
                     (= 1 (length inner-arguments))
                     (equal? formal (plain-symbol (car inner-arguments))))
               (set! warnings
                     (report warnings 'composition 'info
                             (format #f "`(lambda (~a) (~a (~a ~a)))` is `(compose ~a ~a)`"
                                     formal (call-head-name outer) (call-head-name inner) formal
                                     (call-head-name outer) (call-head-name inner))
                             node
                             (format #f "(compose ~a ~a)"
                                     (node->string (node-head outer))
                                     (node->string (node-head inner)))))))))
       ast))
    (reverse warnings)))

;;; ===== let-simplification =====
;;;
;;; Three shapes, all of them the same observation: a `let' is a scope, and a
;;; scope around nothing is nothing.
;;;
;;;   (let ((x e)) x)   is   e
;;;   (let () body)     is   body
;;;   (let* ((x e)) b)  is   (let ((x e)) b)     -- one binding binds once

(define (bindings-of let-node)
  "Return the binding lists of a `let` form, or @code{#f}."
  (let ((arguments (form-arguments let-node)))
    (and (pair? arguments) (node-list? (car arguments))
      (node-elements (car arguments)))))

(define (binding-parts binding)
  "Return @code{(name . value)} for one binding, or @code{#f} when it is not one."
  (and (node-list? binding)
    (let ((parts (node-elements binding)))
      (and (= 2 (length parts))
        (plain-symbol (car parts))
        (cons (plain-symbol (car parts)) (cadr parts))))))

(define (let-simplification-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (let ((head (call-head-name node)))
         (when (memq head '(let let*))
           (let* ((bindings (bindings-of node))
                  (body (let ((arguments (form-arguments node)))
                          (if (pair? arguments) (cdr arguments) '())))
                  (parts (and bindings (map binding-parts bindings))))
             (cond
               ;; (let () body) is body
               [(and (null? bindings) (= 1 (length body)))
                (let ((only (car body)))
                  (set! warnings
                        (report warnings 'let-simplification 'info
                                "a `let` with no bindings is the body; drop the scope"
                                node
                                (substring source (node-index only) (node-end-index only)))))]
               ;; (let ((x e)) x) is e
               [(and bindings (= 1 (length bindings)) (= 1 (length body))
                  (car parts)
                  (plain-symbol (car body))
                  (equal? (plain-symbol (car body)) (car (car parts))))
                (let ((value (cdr (car parts))))
                  (set! warnings
                        (report warnings 'let-simplification 'info
                                (format #f "a `let` that binds `~a` to return it is its value"
                                        (car (car parts)))
                                node
                                (substring source (node-index value)
                                           (node-end-index value)))))]
               ;; (let* ((x e)) body) is (let ((x e)) body)
               [(and (eq? head 'let*) bindings
                  (= 1 (length bindings))
                  (car parts))
                (set! warnings
                      (report warnings 'let-simplification 'info
                              "a `let*` with one binding binds it once; `let` says so"
                              (node-head node)
                              "let"))])))))
     ast)
    (reverse warnings)))

;;; ===== The checkers =====

(define eta-reduction-checker
  (checker 'eta-reduction
           "A lambda that only passes its argument on, where the procedure itself would do"
           '(scheme)
           eta-reduction-warnings
           #:severity 'info
           #:example '((input . "(lambda (x) (f x))")
                       (fix . "f"))))

(define beta-reduction-checker
  (checker 'beta-reduction
           "A lambda applied in place, where a `let` would say the same thing once"
           '(scheme elisp cl)
           beta-reduction-warnings
           #:severity 'info
           #:example '((input . "((lambda (x) (f x)) (g))")
                       (fix . "(let ((x (g))) (f x))"))))

(define composition-checker
  (checker 'composition
           "A lambda whose body is one call around another, where `compose` names the pipeline"
           '(scheme)
           composition-warnings
           #:severity 'info
           #:example '((input . "(lambda (x) (f (g x)))")
                       (fix . "(compose f g)"))))

(define let-simplification-checker
  (checker 'let-simplification
           "A `let` that binds a name to return it, binds nothing, or binds once with `let*`"
           'all
           let-simplification-warnings
           #:severity 'info
           #:example '((input . "(let ((x (f))) x)")
                       (fix . "(f)"))))

;;; ===== The list =====

(define simplify-checkers
  (list eta-reduction-checker
        beta-reduction-checker
        composition-checker
        let-simplification-checker))
