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
  (let ((max-depth (or (option 'max-depth 4)))
        (max-elements (or (option 'max-elements 12)))
        (warnings '()))
    (define (report node message)
      (set! warnings
        (cons (make-lint-warning 'wrap-in-let 'info message (node-start node))
          warnings)))
    (for-each-node
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
                           (format #f "this body form nests ~a levels deep; bind part of it in a `let` (or `let*` when the bindings depend on each other)"
                             depth))]
                        [(> elements max-elements)
                         (report form
                           (format #f "this body form has ~a elements; bind part of it in a `let` (or `let*` when the bindings depend on each other)"
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
    #:example '((input . "(define (f x)\n  (list (car (cdr (assq 'a (cdr x)))))\n  (car (cdr (assq 'b (cdr x))))))")
                (fix . "no automatic fix; bind the repeated part in a `let`"))))

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
    (for-each-node
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
                         (format #f "`~a` takes ~a positional arguments; consider `:key` arguments for the ones that are not obvious"
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
                (fix . "no automatic fix; name the arguments in the callee and pass them as keywords"))))

;;; ===== The list =====

(define ergonomics-checkers
  (list wrap-in-let-checker keyword-arguments-checker))
