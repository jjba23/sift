;; sift --- Common Lisp checkers
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Common Lisp names symbols through packages, and the Google Common Lisp
;; style guide asks production code to say which package a symbol comes from
;; instead of relying on the reader's current one.  These checkers report a
;; missing package prefix, a `::` access to an internal symbol, a missing
;; docstring, a LOOP that mixes keyword spellings, and two forms that say
;; nothing their argument does not already say.

(define-module (sift checkers cl)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift check base)
  #:export (cl-checkers
            package-prefix-checker
            double-colon-checker
            defun-no-docstring-checker
            loop-keyword-style-checker
            redundant-identity-checker
            defpackage-use-checker
            ecase-otherwise-checker
            mixed-lambda-list-checker))

;;; ===== package-prefix =====

(define (exported-symbols ast)
  "Return the symbol names a file's `defpackage` forms export."
  (let ((exported '()))
    (for-each-node
     (lambda (node)
       (when (list-headed-by? node 'defpackage)
         (for-each
          (lambda (clause)
            (when (and (node-list? clause)
                    (memq (atom-symbol (node-head clause)) '(export :export)))
              (for-each (lambda (element)
                          (let ((name (atom-symbol element)))
                            (when name
                              (set! exported (cons name exported)))))
                        (form-arguments clause))))
          (form-arguments node))))
     ast)
    (delete-duplicates exported)))

(define (inside-defpackage? node ancestors)
  (any (lambda (ancestor) (list-headed-by? ancestor 'defpackage)) ancestors))

(define (package-prefix-warnings ast source dialect)
  (let ((exported (exported-symbols ast))
        (warnings '()))
    (when (pair? exported)
      (walk-forms
       (lambda (node ancestors)
         (when (and (node-list? node)
                 (not (inside-defpackage? node ancestors)))
           (let ((head (node-head node)))
             (when (and head
                     (node-atom? head)
                     (not (node-package-marker head))
                     (memq (atom-symbol head) exported))
               (set! warnings
                     (cons (make-lint-warning
                            'package-prefix 'warning
                            (format #f "exported symbol `~a` is used without a package prefix"
                                    (node-value head))
                            (node-start head))
                           warnings))))))
       ast))
    (reverse warnings)))

(define package-prefix-checker
  (checker 'package-prefix
           "Exported symbols referenced without a package prefix"
           '(cl)
           package-prefix-warnings
           #:severity 'warning
           #:example '((input . "(defpackage :app (:export #:run))\n(run)")
                       (fix . "no automatic fix; write `app:run`"))))

;;; ===== double-colon =====

(define (double-colon-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
     (lambda (node)
       (when (node-atom? node)
         (let ((marker (node-package-marker node)))
           (when (and marker (string-suffix? "::" marker))
             (set! warnings
                   (cons (make-lint-warning
                          'double-colon 'warning
                          (format #f "`~a` reaches into another package's internals"
                                  (node-value node))
                          (node-start node))
                         warnings))))))
     ast)
    (reverse warnings)))

(define double-colon-checker
  (checker 'double-colon
           "Use of `package::internal-symbol` in production code"
           '(cl)
           double-colon-warnings
           #:severity 'warning
           #:example '((input . "(other::internal-thing 1)")
                       (fix . "no automatic fix; export the symbol or use its public name"))))

;;; ===== defun-no-docstring =====

(define (defun-no-docstring-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (let ((head (and (node-list? node) (node-head-name node))))
         (when (and head
                 (memq head '(defun defmacro defgeneric defmethod))
                 (not (docstring-node node)))
           (set! warnings
                 (cons (make-lint-warning
                        'defun-no-docstring 'info
                        (format #f "`~a` form has no docstring" head)
                        (node-start node))
                       warnings)))))
     ast)
    (reverse warnings)))

(define defun-no-docstring-checker
  (checker 'defun-no-docstring
           "`defun`, `defmacro` or `defgeneric` without a docstring"
           '(cl)
           defun-no-docstring-warnings
           #:severity 'info
           #:example '((input . "(defun f (x) (+ x 1))")
                       (fix . "no automatic fix; write the docstring"))))

;;; ===== loop-keyword-style =====

(define loop-keywords
  '(for in on do doing collect collecting append appending nconc nconcing
        sum summing count counting maximize maximizing minimize minimizing
        when unless if else end while until repeat initially finally
        return with being the of from below above to upto downto by then across
        into named thereis always never))

(define (loop-form? node)
  (list-headed-by? node 'loop))

(define (loop-keyword-style-warnings ast source dialect)
  (let ((style (or (option 'loop-keywords 'either) 'either))
        (warnings '()))
    (unless (eq? style 'either)
      (for-each-form
       (lambda (node)
         (when (loop-form? node)
           (for-each
            (lambda (element)
              (when (node-atom? element)
                (let* ((text (node-value element))
                       (keyword-text (string-append ":" text))
                       (symbolic-text (if (string-prefix? ":" text)
                                        (substring text 1)
                                        text))
                       (wanted (if (eq? style 'literal)
                                 keyword-text
                                 symbolic-text))
                       (wrong? (if (eq? style 'literal)
                                 (and (eq? (node-atom-type element) 'symbol)
                                   (memq (atom-symbol element) loop-keywords))
                                 (and (eq? (node-atom-type element) 'keyword)
                                   (memq (atom-symbol element) loop-keywords)))))
                  (when wrong?
                    (set! warnings
                          (cons (make-lint-warning/range
                                 'loop-keyword-style 'info
                                 (format #f "LOOP keyword `~a` should be written `~a`"
                                         text wanted)
                                 (node-start element)
                                 (node-index element) (node-end-index element)
                                 (lambda (source start end) wanted))
                                warnings))))))
            (form-arguments node))))
       ast))
    (reverse warnings)))

(define loop-keyword-style-checker
  (checker 'loop-keyword-style
           "Symbolic LOOP keywords where the project style asks for literal `:keywords`"
           '(cl)
           loop-keyword-style-warnings
           #:severity 'info
           #:example '((input . "(loop for x in xs do (print x))")
                       (fix . "(loop :for x :in xs :do (print x))"))))

;;; ===== redundant-identity =====

(define (redundant-identity-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (cond
         [(and (list-headed-by? node 'identity)
            (= 1 (argument-count node)))
          (set! warnings
                (cons (make-lint-warning 'redundant-identity 'info
                                         "`identity` here adds nothing"
                                         (node-start node))
                      warnings))]
         [(and (list-headed-by? node 'funcall)
            (pair? (form-arguments node))
            (let ((first (car (form-arguments node))))
              (and (node-prefix? first)
                (equal? (node-token first) "#'"))))
          (set! warnings
                (cons (make-lint-warning 'redundant-identity 'info
                                         "`funcall` of a function name can be a direct call"
                                         (node-start node))
                      warnings))]))
     ast)
    (reverse warnings)))

(define redundant-identity-checker
  (checker 'redundant-identity
           "`(identity x)` or `(funcall #'foo ...)` where the value suffices"
           '(cl)
           redundant-identity-warnings
           #:severity 'info
           #:example '((input . "(mapcar (lambda (x) (identity x)) xs)")
                       (fix . "no automatic fix; drop the call"))))

;;; ===== defpackage-use =====
;;;
;;; `(:use ...)' puts every external symbol of a package into the reader's
;;; scope, so a name that arrives from it cannot be traced to the line that
;;; chose it.  The Google style guide asks for `:import-from' instead, and
;;; exempts `:cl' itself -- a file that has to write `cl:car' is not more
;;; readable for it.

(define (defpackage-options node)
  "Return the options of a `defpackage` form @var{node}, or @code{'()}."
  (if (list-headed-by? node 'defpackage) (form-arguments node) '()))

(define (package-option-name node)
  "Return the name of a `defpackage` option, with its colon dropped."
  (atom-symbol (node-head node)))

(define (defpackage-use-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
     (lambda (node)
       (for-each
        (lambda (option)
          (when (and (node-list? option)
                  (eq? (package-option-name option) 'use))
            (for-each
             (lambda (package)
               (let ((name (atom-symbol package)))
                 (when (and name (not (memq name '(cl common-lisp))))
                   (set! warnings
                         (cons (make-lint-warning
                                'defpackage-use 'warning
                                (format #f "`:use` of `~a` puts every name it exports in scope; `:import-from` names them one at a time"
                                        name)
                                (node-start package))
                               warnings)))))
             (form-arguments option))))
        (defpackage-options node)))
     ast)
    (reverse warnings)))

(define defpackage-use-checker
  (checker 'defpackage-use
           "A `defpackage` that `:use`s a package other than `:cl`"
           '(cl)
           defpackage-use-warnings
           #:severity 'warning
           #:example '((input . "(defpackage #:demo (:use #:cl #:alexandria))")
                       (fix . "(defpackage #:demo (:use #:cl) (:import-from #:alexandria #:with-gensyms))"))))

;;; ===== ecase-otherwise =====
;;;
;;; `ecase' and `etypecase' exist so that the implementation can complain when
;;; the clauses do not cover every case.  A default clause is the one thing
;;; that turns that complaint off, and `case' and `typecase' are the forms
;;; that say "there is a default" in the first place.

(define (ecase-otherwise-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (when (head-is-one-of? node '(ecase etypecase ccase ctypecase))
         (for-each
          (lambda (clause)
            (let ((head (and (node-list? clause) (node-head clause))))
              (when (and head (node-atom? head)
                      (memq (atom-symbol head) '(t otherwise)))
                (set! warnings
                      (cons (make-lint-warning
                             'ecase-otherwise 'warning
                             "a default clause turns off the exhaustiveness check the form exists for"
                             (node-start clause))
                            warnings)))))
          (form-arguments node))))
     ast)
    (reverse warnings)))

(define ecase-otherwise-checker
  (checker 'ecase-otherwise
           "A default clause in `ecase`, `etypecase`, `ccase` or `ctypecase`"
           '(cl)
           ecase-otherwise-warnings
           #:severity 'warning
           #:example '((input . "(ecase x (a 1) (t 2))")
                       (fix . "(case x (a 1) (t 2))"))))

;;; ===== mixed-lambda-list =====
;;;
;;; A lambda list with `&optional' before `&key' accepts both, and which kind
;;; an argument is depends on how many the caller passed: the optional
;;; parameters take their arguments first, so the first keyword a caller
;;; writes is read as an optional one.  One of the two is enough.

(define (lambda-list-formals node)
  "Return the lambda list of @var{node} when it has one, or @code{#f}."
  (let ((head (and (node-list? node) (node-head-name node))))
    (and (memq head '(lambda defun defmacro defsubst defmethod defgeneric
                       cl-defun cl-defmacro cl-defsubst cl-defmethod
                       cl-defgeneric))
      (let ((formals (formals-of-form node)))
        (and (node-list? formals) formals)))))

(define (lambda-list-marker node)
  "Return the name of the lambda-list marker at @var{node}, or @code{#f}."
  (and (node-atom? node) (eq? (node-atom-type node) 'symbol)
    (let ((name (node-value node)))
      (and (> (string-length name) 1)
        (char=? (string-ref name 0) #\&)
        name))))

(define (mixed-lambda-list-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-form
     (lambda (node)
       (let ((formals (lambda-list-formals node)))
         (when formals
           (let ((markers (filter-map lambda-list-marker (form-elements formals))))
             (when (and (member "&optional" markers) (member "&key" markers))
               (set! warnings
                     (cons (make-lint-warning
                            'mixed-lambda-list 'warning
                            "a lambda list with both `&optional` and `&key` reads a caller's first keyword as an optional argument"
                            (node-start formals))
                           warnings)))))))
     ast)
    (reverse warnings)))

(define mixed-lambda-list-checker
  (checker 'mixed-lambda-list
           "A lambda list with both `&optional` and `&key`"
           '(cl)
           mixed-lambda-list-warnings
           #:severity 'warning
           #:example '((input . "(defun f (x &optional y &key z) (list x y z))")
                       (fix . "(defun f (x &key y z) (list x y z))"))))

;;; ===== The list =====

(define cl-checkers
  (list package-prefix-checker
        double-colon-checker
        defun-no-docstring-checker
        loop-keyword-style-checker
        redundant-identity-checker
        defpackage-use-checker
        ecase-otherwise-checker
        mixed-lambda-list-checker))
