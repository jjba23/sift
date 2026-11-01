;; sift --- body checkers
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Forms whose body is missing, or whose body is a `progn' that need not be
;; there.  A body that is empty is usually a form that was written for its
;; side effect and then edited; a body of one `progn' is a form that was
;; spliced by hand and never cleaned up.

(define-module (sift checkers bodies)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift check base)
  #:export (bodies-checkers
            empty-body-checker
            redundant-progn-checker
            when-without-body-checker
            unless-without-body-checker
            negated-if-checker
            empty-form-checker
            duplicate-keyword-checker
            duplicate-binding-checker))

;;; ===== empty-body =====

(define binding-heads
  '(let let* letrec letrec* let-values let*-values let-syntax letrec-syntax
     flet labels macrolet symbol-macrolet))

(define (cond-clauses node)
  (filter node-list? (form-arguments node)))

(define (empty-body-warnings ast source dialect)
  (let ((warnings '()))
    (define (warn node message)
      (set! warnings
            (cons (make-lint-warning 'empty-body 'warning message
                                     (node-start node))
                  warnings)))
    (walk-forms
     (lambda (node ancestors)
       (let ((head (and (node-list? node)
                     (node-head-name node))))
         (case head
           [(cond)
            (for-each (lambda (clause)
                        (when (< (length (form-elements clause)) 2)
                          (warn clause "`cond` clause has no body")))
                      (cond-clauses node))
            (when (null? (cond-clauses node))
              (warn node "`cond` form has no clauses"))]
           [(let let* letrec letrec* flet labels)
            (when (< (length (form-elements node)) 3)
              (warn node (format #f "`~a` form has no body" head)))]
           [(lambda)
            (when (< (length (form-elements node)) 3)
              (warn node "`lambda` form has no body"))]
           [(when unless)
            ;; Emacs Lisp has no dedicated `when-without-body' checker, so this
            ;; one covers it; in Scheme and Common Lisp the dedicated checker
            ;; reports it and this one stays quiet, to keep one finding per
            ;; problem.
            (when (and (eq? dialect 'elisp)
                    (< (length (form-elements node)) 3))
              (warn node (format #f "`~a` form has no body" head)))])))
     ast)
    (reverse warnings)))

(define empty-body-checker
  (checker 'empty-body
           "A `cond`, `let` or `lambda` form with no body"
           'all
           empty-body-warnings
           #:severity 'warning
           #:example '((input . "(cond ((= x 1)))")
                       (fix . "no automatic fix; write the body the clause needs"))))

;;; ===== redundant-progn =====

(define (head-arguments-to-skip head)
  "Return how many elements of a form come before its body, per operator."
  (case head
    [(lambda lambda* cl-lambda) 1]
    [(let let* letrec letrec* let-values let*-values
       flet labels macrolet symbol-macrolet) 2]
    [(define define* define-public) 2]
    ;; the head, the name and the argument list come before the body of a
    ;; function definition, so three elements are not body forms
    [(defun defmacro defsubst cl-defun cl-defmacro cl-defsubst
            defmethod defgeneric) 3]
    [else #f]))

(define (redundant-progn-warnings ast source dialect)
  (let ((warnings '()))
    (define (check node)
      (let* ((head (and (node-list? node) (node-head-name node)))
             (skip (and head (head-arguments-to-skip head))))
        (when skip
          (let ((body (body-elements node skip)))
            (when (and (pair? body)
                    (null? (cdr body))
                    (list-headed-by? (car body) 'progn))
              (let* ((progn (car body))
                     (arguments (form-arguments progn))
                     (replacement
                      (string-join
                       (map (lambda (arg) (node-source source arg)) arguments)
                       " ")))
                (set! warnings
                      (cons (make-lint-warning/range
                             'redundant-progn 'warning
                             (format #f "redundant `progn` as the only body of `~a`"
                                     head)
                             (node-start progn)
                             (node-index progn) (node-end-index progn)
                             (lambda (source start end) replacement))
                            warnings))))))))
    (for-each-form check ast)
    (reverse warnings)))

(define redundant-progn-checker
  (checker 'redundant-progn
           "`progn` used as the sole body of a form that already has an implicit `progn`"
           'all
           redundant-progn-warnings
           #:severity 'warning
           #:example '((input . "(define (f x) (progn (a) (b)))")
                       (fix . "splice the `progn` body into the enclosing form"))))

;;; ===== when-without-body / unless-without-body =====

(define (without-body-warnings checker-name head-name)
  (lambda (ast source dialect)
    (let ((warnings '()))
      (walk-forms
       (lambda (node ancestors)
         (when (and (list-headed-by? node head-name)
                 (< (length (form-elements node)) 3))
           (set! warnings
                 (cons (make-lint-warning checker-name 'warning
                                          (format #f "`~a` form has no body"
                                                  head-name)
                                          (node-start node))
                       warnings))))
       ast)
      (reverse warnings))))

(define when-without-body-checker
  (checker 'when-without-body
           "`when` form with no body"
           '(scheme cl)
           (without-body-warnings 'when-without-body 'when)
           #:severity 'warning
           #:example '((input . "(when (ready?))")
                       (fix . "no automatic fix; add the body or drop the form"))))

(define unless-without-body-checker
  (checker 'unless-without-body
           "`unless` form with no body"
           '(scheme cl)
           (without-body-warnings 'unless-without-body 'unless)
           #:severity 'warning
           #:example '((input . "(unless (ready?))")
                       (fix . "no automatic fix; add the body or drop the form"))))

;;; ===== negated-if =====

(define (negation-test node)
  "Return the test inside @code{(not test)}, or @code{#f}."
  (and (list-headed-by? node 'not)
    (= 1 (argument-count node))
    (car (form-arguments node))))

(define (negated-if-warnings ast source dialect)
  (let ((warnings '()))
    (define (report node test replacement)
      (set! warnings
            (cons (make-lint-warning/range 'negated-if 'warning
                                           (format #f "`~a` with a negated test is an `unless`"
                                                   (node-head-name node))
                                           (node-start node)
                                           (node-index node) (node-end-index node)
                                           (lambda (source start end) replacement))
                  warnings)))
    (for-each-form
     (lambda (node)
       (when (node-list? node)
         (let ((head (node-head-name node))
               (arguments (form-arguments node)))
           (when (and (memq head '(if when))
                   (pair? arguments))
             (let ((test (negation-test (car arguments))))
               (when test
                 (let* ((rest (cdr arguments))
                        (pieces (map (lambda (argument)
                                       (node-source source argument))
                                     rest)))
                   (report node test
                           (string-append "(unless "
                                          (node-source source test)
                                          (if (null? pieces)
                                            ""
                                            (string-append " " (string-join pieces " ")))
                                          ")")))))))))
     ast)
    (reverse warnings)))

(define negated-if-checker
  (checker 'negated-if
           "`if` or `when` with a negated test, which `unless` says directly"
           '(scheme elisp cl)
           negated-if-warnings
           #:severity 'warning
           #:example '((input . "(if (not (ready?)) (wait) (go))")
                       (fix . "(unless (ready?) (wait) (go))"))))

;;; ===== empty-form =====

;; Forms whose first argument is a parameter list, where `()` is the empty
;; one: the rule is about `()` in *value* position, and an empty formal list
;; is not a value, it is how a procedure that takes no argument says so.
(define parameter-list-heads
  '(lambda lambda* define define* define-public let let* letrec letrec*
     let-values let*-values case-lambda do guard parameterize
     with-exception-handler))

;; Forms whose arguments are literals, patterns and templates rather than
;; expressions.  `()` is the empty literals list in `(syntax-rules () ...)`
;; and in `(syntax-case stx () ...)`, and an empty list in a template is data
;; the macro is building -- not a value the program forgot to quote.
(define data-argument-heads
  '(syntax-rules syntax-case))

(define (empty-form-warnings ast source dialect)
  (let ((warnings '()))
    (walk-with-ancestors
     (lambda (node ancestors)
       (when (and (node-list? node)
               (null? (node-elements node))
               (not (quoted-context? ancestors))
               (pair? ancestors))
         (let ((parent (car ancestors)))
           ;; `()` is suspicious as an argument to a call, and nowhere else:
           ;; a parameter list, a binding list, the literals of a transformer
           ;; and quoted data all use it legitimately.
           (when (and (node-list? parent)
                   (node-head-name parent)
                   (not (memq (node-head-name parent) parameter-list-heads))
                   (not (memq (node-head-name parent) data-argument-heads)))
             (set! warnings
                   (cons (make-lint-warning 'empty-form 'warning
                                            "`()` is not a value here; Scheme's empty list needs quoting"
                                            (node-start node))
                         warnings))))))
     ast)
    (reverse warnings)))

(define empty-form-checker
  (checker 'empty-form
           "`()` used where a value is expected"
           '(scheme)
           empty-form-warnings
           #:severity 'warning
           #:example '((input . "(define (f) (list ()))")
                       (fix . "no automatic fix; write `'()` or the value the form should return"))))

;;; ===== duplicate-keyword =====

(define (keyword-name-of node)
  (and (node-atom? node)
    (eq? (node-atom-type node) 'keyword)
    (node-value node)))

(define (duplicate-keyword-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
     (lambda (node)
       (when (node-list? node)
         (let ((seen '()))
           (for-each
            (lambda (element)
              (let ((name (keyword-name-of element)))
                (when name
                  (if (member name seen)
                    (set! warnings
                          (cons (make-lint-warning 'duplicate-keyword 'warning
                                                   (format #f "`~a` is given twice in this call; the second one wins"
                                                           name)
                                                   (node-start element))
                                warnings))
                    (set! seen (cons name seen))))))
            (node-elements node)))))
     ast)
    (reverse warnings)))

(define duplicate-keyword-checker
  (checker 'duplicate-keyword
           "A keyword argument given more than once in the same call"
           'all
           duplicate-keyword-warnings
           #:severity 'warning
           #:example '((input . "(make-window :width 10 :width 20)")
                       (fix . "no automatic fix; deciding which value was meant is the author's"))))

;;; ===== duplicate-binding =====

(define binding-form-heads
  '(let let* letrec letrec* flet labels macrolet symbol-macrolet
     let-values let*-values))

(define (binding-name binding)
  "Return the name a binding binds, or @code{#f}."
  (cond
    [(node-atom? binding)
     (and (eq? (node-atom-type binding) 'symbol) (node-value binding))]
    [(and (node-list? binding) (pair? (node-elements binding)))
     (let ((head (car (node-elements binding))))
       (and (node-atom? head) (node-value head)))]
    [else #f]))

(define (duplicate-binding-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
     (lambda (node)
       (when (and (node-list? node)
               (memq (node-head-name node) binding-form-heads))
         (let* ((arguments (form-arguments node))
                (bindings (and (pair? arguments) (car arguments))))
           (when (node-list? bindings)
             (let ((seen '()))
               (for-each
                (lambda (binding)
                  (let ((name (binding-name binding)))
                    (when name
                      (if (member name seen)
                        (let ((message
                               (format #f "`~a` is bound twice in one `~a`"
                                       name (node-head-name node))))
                          (set! warnings
                                (cons (make-lint-warning 'duplicate-binding
                                                         'warning message (node-start binding))
                                      warnings)))
                        (set! seen (cons name seen))))))
                (node-elements bindings)))))))
     ast)
    (reverse warnings)))

(define duplicate-binding-checker
  (checker 'duplicate-binding
           "The same name bound twice in one binding list"
           'all
           duplicate-binding-warnings
           #:severity 'warning
           #:example '((input . "(let ((x 1) (x 2)) x)")
                       (fix . "no automatic fix; one of the two bindings is a mistake"))))

;;; ===== The list =====

(define bodies-checkers
  (list empty-body-checker
        redundant-progn-checker
        when-without-body-checker
        unless-without-body-checker
        negated-if-checker
        empty-form-checker
        duplicate-keyword-checker
        duplicate-binding-checker))
