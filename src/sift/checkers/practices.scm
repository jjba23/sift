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
            defcustom-without-type-checker))

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

;;; ===== too-many-arguments =====

(define formal-markers
  '(&optional &rest &key &aux &body &whole))

(define (formal-marker? node)
  "Return @code{#t} when @var{node} is a keyword that structures a formal list.

Guile's @code{#!optional} and Common Lisp's @code{&optional} are both spelled
here, and the first is matched by its prefix because writing it out would open
a @code{#!} comment in the very file that lists it."
  (let ((name (and (node-atom? node) (atom-symbol node))))
    (and name
      (or (memq name formal-markers)
        (string-prefix? "#!" (symbol->string name))))))

(define (formal-names nodes)
  "Return the atoms of @var{nodes} that name a parameter."
  (remove formal-marker? nodes))

(define (definition-name-and-formals node)
  "Return @code{(name . formals)} for a definition form, or @code{#f}.

Only the forms whose name is followed by a parameter list are here: a
`define' of a value has no arguments to count, and a lambda is counted
where it is bound rather than where it is written."
  (let* ((head (and (node-list? node) (node-head-name node)))
         (arguments (and head (form-arguments node)))
         (first (and (pair? arguments) (car arguments)))
         (second (and (pair? arguments) (pair? (cdr arguments)) (cadr arguments))))
    (cond
      [(not head) #f]
      [(memq head '(define define* define-public))
       ;; a list's elements already begin with its head, so the parameters
       ;; are what follows the name; `(define ())' has no parameters at all
       (and (node-list? first)
         (pair? (form-elements first))
         (cons (node-head first) (cdr (form-elements first))))]
      [(memq head '(defun defmacro defsubst cl-defun cl-defmacro defsubst
                          defmethod defgeneric))
       (and (node-atom? first) (node-list? second)
         (pair? (form-elements second))
         (cons first (cdr (form-elements second))))]
      [else #f])))

(define (too-many-arguments-warnings ast source dialect)
  (let ((limit (or (option 'max-arguments 5)))
        (warnings '()))
    (for-each-node
     (lambda (node)
       (let ((found (definition-name-and-formals node)))
         (when found
           (let ((count (length (formal-names (cdr found))))
                 (name (car found)))
             (when (> count limit)
               (set! warnings
                     (cons (make-lint-warning
                            'too-many-arguments 'info
                            (format #f "this definition takes ~a arguments; beyond ~a the call site stops reading as a sentence"
                                    count limit)
                            (and name (node-start name)))
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

(define practices-checkers
  (list quote-of-constant-checker
        eq-on-literal-checker
        cond-else-not-last-checker
        duplicate-case-datum-checker
        too-many-arguments-checker
        defcustom-without-type-checker))
