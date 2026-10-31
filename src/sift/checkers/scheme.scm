;; sift --- Scheme checkers
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

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
  #:use-module (sift check base)
  #:export (scheme-checkers
              internal-define-checker
              if-without-else-checker
              letrec-single-binding-checker
              quasiquote-unquote-constant-checker))

;;; ===== internal-define =====

(define (procedure-form? node)
  "Return @code{#t} when @var{node} introduces a procedure body.

That is a @code{lambda}, or a @code{define} whose first argument is the
@code{(name . formals)} pair.  A body of either may start with definitions:
Scheme has always allowed that, and an internal definition outside one is a
definition that will run in the wrong scope."
  (cond
    ((list-headed-by? node 'lambda) #t)
    ((list-headed-by? node 'define)
     (let ((first (and (pair? (form-arguments node)) (car (form-arguments node)))))
       (and first (node-list? first))))
    (else #f)))

(define (internal-define-warnings ast source dialect)
  (let ((warnings '()))
    (walk-with-ancestors
       (lambda (node ancestors)
         (when (list-headed-by? node 'define)
           ;; The nearest enclosing form decides: a `let' in between is the
           ;; nested scope the author meant, so the definition is fine.
           (let ((owner (find (lambda (ancestor)
                                (or (procedure-form? ancestor)
                                  (head-is-one-of? ancestor
                                    '(let let* letrec letrec*
                                       parameterize))))
                          ancestors)))
             (when (and owner (procedure-form? owner))
               (set! warnings
                 (cons (make-lint-warning
                          'internal-define 'warning
                          "`define` inside a procedure body"
                          (node-start node))
                   warnings))))))
       ast)
    (reverse warnings)))

(define internal-define-checker
  (checker 'internal-define
    "`define` inside a procedure body without an enclosing `let`"
    '(scheme)
    internal-define-warnings
    #:severity 'warning
    #:example '((input . "(define (f)\n  (define y 1)\n  (if y 2 3))")
                (fix . "no automatic fix; use `let' for a local binding"))))

;;; ===== if-without-else =====

(define (if-without-else-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
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
    (for-each-node
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
                              (format #f "`~a` with a single binding that does not refer to itself; `let` is enough"
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
        ((and (node-prefix? inner) (equal? (node-token inner) ","))
         (let ((child (node-child inner)))
           (when (and child (constant-atom? child)) (report inner))))
        ((list-headed-by? inner 'unquote)
         (let ((args (form-arguments inner)))
           (when (and (pair? args) (constant-atom? (car args))) (report inner))))
        ((node-list? inner)
         (for-each check-quasiquote (node-elements inner)))))
    (for-each-node
       (lambda (node)
         (cond
           ((and (node-prefix? node) (equal? (node-token node) "`"))
            (let ((child (node-child node)))
              (when child (check-quasiquote child))))
           ((list-headed-by? node 'quasiquote)
            (let ((args (form-arguments node)))
              (when (pair? args) (check-quasiquote (car args)))))))
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

;;; ===== The list =====

(define scheme-checkers
  (list internal-define-checker
    if-without-else-checker
    letrec-single-binding-checker
    quasiquote-unquote-constant-checker))
