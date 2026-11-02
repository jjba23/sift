;; sift --- naming
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A Lisp has no type declarations to lean on, so a name is most of what a
;; reader knows about a value: that it is a predicate, that it is special,
;; that it is a constant.  These three rules are about the name rather than
;; about the code under it.
;;
;; None of them has a fix.  Renaming a definition is a refactor: it changes
;; every call site in every file, and a formatter that renames one of them has
;; broken the program.  So the rules report, and the author decides.

(define-module (sift checkers naming)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (sift strings)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift check base)
  #:export (naming-checkers
            naming-convention-checker
            predicate-name-checker
            special-variable-name-checker
            single-letter-name-checker))

;;; ===== Reading a definition =====

(define (definition-name-node node)
  "Return the atom that names the definition @var{node}, or @code{#f}.

The name of `(define (f x) ...)' is inside the argument list; the name of
`(defun f (x) ...)' is the first argument.  A `define' of a value has the same
shape as the second."
  (let* ((head (and (node-list? node) (node-head-name node)))
         (arguments (form-arguments node))
         (first (and (pair? arguments) (car arguments))))
    (cond
      [(not head) #f]
      [(memq head '(define define* define-public define-syntax define-inlinable))
       (cond
         [(and first (node-list? first))
          (let ((inner (node-head first)))
            (and inner (node-atom? inner) inner))]
         [(and first (node-atom? first)) first]
         [else #f])]
      [(memq head '(defun defmacro defsubst cl-defun cl-defmacro cl-defsubst
                          defmethod defgeneric defvar defconst defcustom defparameter
                          defconstant defface defalias define-minor-mode
                          define-derived-mode))
       (and first (node-atom? first) first)]
      [else #f])))

(define (definition-kind node)
  "Return the operator of a definition @var{node}, as a symbol."
  (node-head-name node))

(define (symbol-text node)
  (and node (node-atom? node) (eq? (node-atom-type node) 'symbol)
    (node-value node)))

;;; ===== naming-convention =====

(define (camel-case? name)
  "Return @code{#t} when @var{name} has an uppercase letter after a
lowercase one, which is camelCase and not Lisp."
  (let loop ((chars (string->list name)) (previous #f))
    (cond
      [(null? chars) #f]
      [(and previous
         (char-lower-case? previous)
         (char-upper-case? (car chars)))
       #t]
      [else (loop (cdr chars) (car chars))])))

(define (starts-upper? name)
  (and (> (string-length name) 0)
    (char-upper-case? (string-ref name 0))))

(define (operatorish? name)
  "Return @code{#t} for a name that is punctuation rather than a word."
  (every-char (lambda (c)
                (not (or (char-alphabetic? c) (char-numeric? c))))
              name))

(define (naming-convention-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (let ((name-node (definition-name-node node)))
         (let ((name (symbol-text name-node)))
           (when (and name (not (operatorish? name)))
             (cond
               [(string-index name #\_)
                (set! warnings
                      (cons (make-lint-warning 'naming-convention 'warning
                                               (format #f "`~a` has an underscore; Lisp names use hyphens"
                                                       name)
                                               (node-start name-node))
                            warnings))]
               [(camel-case? name)
                (set! warnings
                      (cons (make-lint-warning 'naming-convention 'warning
                                               (format #f "`~a` is camelCase; Lisp names use hyphens"
                                                       name)
                                               (node-start name-node))
                            warnings))]
               [(and (starts-upper? name)
                  ;; `Foo' is a class in Common Lisp and a mistake in the
                  ;; other two; a one-letter name is not worth a finding
                  (> (string-length name) 1))
                (set! warnings
                      (cons (make-lint-warning 'naming-convention 'warning
                                               (format #f "`~a` starts with a capital; Lisp names are lower case"
                                                       name)
                                               (node-start name-node))
                            warnings))])))))
     ast)
    (reverse warnings)))

(define naming-convention-checker
  (checker 'naming-convention
           "An underscore or camelCase in the name of a definition"
           'all
           naming-convention-warnings
           #:severity 'warning
           #:example '((input . "(define (read_file path) ...)")
                       (fix . "no automatic fix; renaming changes every call site"))))

;;; ===== predicate-name =====

;; Calls that answer a question.  `and' and `or' are not here: they return
;; whichever of their arguments decided the answer, which is often a value.
(define boolean-operators
  '(= < > <= >= eq? eqv? equal? not null? pair? list? number? string? symbol?
      boolean? char? vector? procedure? zero? positive? negative? odd? even?
      string=? string<? string>? memq memv member assq assv assoc
      null empty? any every
      eq eql equal null atom consp listp stringp numberp symbolp functionp
      zerop evenp oddp plusp minusp endp))

(define (predicate-name-of? node dialect)
  "Return @code{#t} when the name of @var{node} says it is a predicate."
  (let* ((name (symbol-text (definition-name-node node)))
         (suffix (if (eq? dialect 'elisp) "-p" "?")))
    (and name
      (or (string-suffix? "?" name)
        (string-suffix? "-p" name)
        (string-suffix? suffix name)))))

(define (boolean-returning? node)
  "Return @code{#t} when the body of @var{node} answers a question.

Only the *last* body form is looked at, because that is the value the
procedure returns: a procedure whose last form is `(and (> x 0) (< x 10))'
is a predicate, and one whose last form is `(list x)' is not."
  (let* ((head (definition-kind node))
         (skip (case head
                 [(lambda lambda*) 1]
                 [(define define* define-public) 2]
                 [(defun defmacro defsubst cl-defun cl-defmacro defmethod
                         defgeneric) 3]
                 [else 2]))
         (body (body-elements node skip)))
    (let ((last-form (and (pair? body) (car (reverse body)))))
      (and (node-list? last-form)
        (let ((operator (node-head-name last-form)))
          (and operator
            (or (memq operator boolean-operators)
              ;; a procedure that calls a predicate is one
              (let ((text (symbol->string operator)))
                (or (string-suffix? "?" text)
                  (string-suffix? "-p" text))))))))))

(define (predicate-name-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (when (and (definition-name-node node)
               (boolean-returning? node)
               (not (predicate-name-of? node dialect)))
         (set! warnings
               (cons (make-lint-warning 'predicate-name 'info
                                        (format #f "`~a` returns a boolean; name it `~a~a`"
                                                (symbol-text (definition-name-node node))
                                                (symbol-text (definition-name-node node))
                                                (if (eq? dialect 'elisp) "-p" "?"))
                                        (node-start (definition-name-node node)))
                     warnings))))
     ast)
    (reverse warnings)))

(define predicate-name-checker
  (checker 'predicate-name
           "A procedure that answers a question but is not named like one"
           'all
           predicate-name-warnings
           #:severity 'info
           #:example '((input . "(define (positive x) (> x 0))")
                       (fix . "no automatic fix; rename it to `positive?`"))))

;;; ===== special-variable-name =====

(define (earmuffed? name)
  (and (> (string-length name) 2)
    (string-prefix? "*" name)
    (string-suffix? "*" name)))

(define (plus-quoted? name)
  (and (> (string-length name) 2)
    (string-prefix? "+" name)
    (string-suffix? "+" name)))

(define (special-variable-name-warnings ast source dialect)
  (let ((warnings '()))
    (define (report node message)
      (set! warnings
            (cons (make-lint-warning 'special-variable-name 'info message
                                     (node-start (definition-name-node node)))
                  warnings)))
    (for-each-form
     (lambda (node)
       (let* ((kind (definition-kind node))
              (name (symbol-text (definition-name-node node))))
         (when (and kind name)
           (case kind
             [(defparameter defvar)
              (cond
                [(and (eq? dialect 'cl) (not (earmuffed? name)))
                 (report node
                         (format #f "`~a` is special; Common Lisp writes it `*~a*`"
                                 name name))]
                ;; Emacs Lisp has no earmuff convention: a dynamic variable
                ;; is simply a variable with a name
                [(and (eq? dialect 'elisp) (earmuffed? name))
                 (report node
                         (format #f "`~a` is earmuffed; Emacs Lisp writes it `~a`"
                                 name (substring name 1 (- (string-length name) 1))))])]
             [(defconstant)
              (unless (plus-quoted? name)
                (report node
                        (format #f "`~a` is a constant; Common Lisp writes it `+~a+`"
                                name name)))]
             [else #f]))))
     ast)
    (reverse warnings)))

(define special-variable-name-checker
  (checker 'special-variable-name
           "A special variable or constant whose name does not say so"
           '(elisp cl)
           special-variable-name-warnings
           #:severity 'info
           #:example '((input . "(defparameter port 8080)")
                       (fix . "no automatic fix; rename it to `*port*`"))))

;;; ===== single-letter-name =====
;;
;; A name is how a reader remembers a value without re-deriving it, and one
;; letter is not a name.  Riastradh allows index variables such as `i' when
;; their scope is a single vector; which letters a project is willing to read
;; that way is not something a linter can decide, so the list is a setting.

(define (single-letter? name)
  (and (= 1 (string-length name))
    (char-alphabetic? (string-ref name 0))
    (not (char=? (string-ref name 0) #\_))))

(define (allowed-single-letter-names)
  "Return the names @code{single-letter-name} accepts, as strings."
  (map (lambda (name)
         (cond [(symbol? name) (symbol->string name)]
               [(string? name) name]
               [else ""]))
       (or (option 'single-letter-names '(i j)) '())))

(define (plain-formals nodes)
  "Return the parameters of a formal list, without rest markers or keywords."
  (filter (lambda (node)
            (and (node-atom? node)
              (eq? (node-atom-type node) 'symbol)
              (not (string-prefix? "&" (node-value node)))
              (not (string-prefix? "#:" (node-value node)))))
          nodes))

(define (binding-name-atom binding)
  "Return the atom that names @var{binding}, or @code{#f}."
  (cond
    [(and (node-atom? binding) (eq? (node-atom-type binding) 'symbol)) binding]
    [(node-list? binding)
     (let ((first (and (pair? (node-elements binding))
                    (car (node-elements binding)))))
       (and first
         (node-atom? first)
         (eq? (node-atom-type first) 'symbol)
         first))]
    [else #f]))

(define (bound-names node)
  "Return the atoms that bind a variable in @var{node}, when it binds any.

Only the places a reader reads as a variable are here: the parameters of a
procedure and the names of a binding list.  A top-level `define' names a
program, not a variable, and renaming it is not this rule's business."
  (let* ((head (and (node-list? node) (node-head-name node)))
         (arguments (form-arguments node))
         (first (and (pair? arguments) (car arguments)))
         (second (and (pair? arguments) (pair? (cdr arguments)) (cadr arguments))))
    (cond
      [(memq head '(let let* letrec letrec* let-values let*-values))
       (filter-map binding-name-atom (form-elements first))]
      [(memq head '(lambda lambda*))
       (plain-formals (form-elements first))]
      [(memq head '(defun defmacro defsubst cl-defun cl-defmacro cl-defsubst
                          defmethod defgeneric))
       (plain-formals (form-elements second))]
      [(memq head '(define define* define-public define-inlinable))
       ;; (define (f x) ...) binds x; (define f 1) binds a name of its own
       (if (and (node-list? first) (pair? (form-elements first)))
         (plain-formals (cdr (form-elements first)))
         '())]
      [else '()])))

(define (single-letter-name-warnings ast source dialect)
  (let ((warnings '())
        (allowed (allowed-single-letter-names)))
    (for-each-form
     (lambda (node)
       (for-each
        (lambda (name-node)
          (let ((name (node-value name-node)))
            (when (and (single-letter? name) (not (member name allowed)))
              (set! warnings
                    (cons (make-lint-warning
                           'single-letter-name 'info
                           (format #f "`~a` is a single-letter name; give it a word, or list it in `single-letter-names`"
                                   name)
                           (node-start name-node))
                          warnings)))))
        (bound-names node)))
     ast)
    (reverse warnings)))

(define single-letter-name-checker
  (checker 'single-letter-name
           "A variable named with a single letter"
           'all
           single-letter-name-warnings
           #:severity 'info
           #:example '((input . "(define (area w h) (* w h))")
                       (fix . "no automatic fix; rename the parameter to a word"))))

;;; ===== The list =====

(define naming-checkers
  (list naming-convention-checker
        predicate-name-checker
        special-variable-name-checker
        single-letter-name-checker))
