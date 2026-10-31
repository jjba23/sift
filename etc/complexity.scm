#!/usr/bin/env guile
!#
;; sift --- complexity report
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; A report of how hard each definition in this tree is to hold in one head,
;; so that refactoring has a target rather than an opinion.
;;
;; Three numbers are reported for every definition, because each answers a
;; different question and no single one is enough:
;;
;;   score  cognitive complexity: every decision costs one, and a decision
;;          nested inside another costs one more.  This is the number that
;;          says "this takes effort to read".
;;   cyc    cyclomatic complexity: one plus the number of decisions.  This is
;;          the number that says "how many paths a test would have to cover".
;;   lines  the length of the definition.  This is the number that says
;;          "how far the reader has to scroll to see the whole thing".
;;
;; Plus the deepest nesting of decisions, which is what `nesting-depth' in
;; the linter reports one form at a time.
;;
;; The definitions are read with sift's own reader, so comments, strings and
;; quoted data are never mistaken for code -- a `(if ...)' in a docstring or
;; an alist key is not a branch.
;;
;; Usage:
;;
;;   guile -L src -L . etc/complexity.scm [FILE-OR-DIRECTORY ...]
;;
;; With no argument the report covers `src'.  The exit status is 0: this is a
;; report, not a gate; `maak complexity' is where the number is compared
;; against a budget, if one is ever wanted.

(use-modules (ice-9 format)
  (srfi srfi-1)
  (srfi srfi-13)
  (sift check base)
  (sift reader)
  (sift records))

;;; ===== What counts as a decision =====

;;; A form that makes the reader hold two or more possibilities at once.  In
;;; a Lisp this is not the same list as the one a brace language would have:
;;; `(let ((x 1)) ...)' is a scope, not a branch, but every `let' clause in a
;;; `cond' is, and `and' short-circuits.  The list below is the union of the
;;; three dialects, because one report over a mixed tree is more useful than
;;; three reports.
(define decision-heads
  '(if when unless cond case case-lambda and or
     let let* letrec letrec* let-values let*-values
     do loop dolist dotimes while
     guard condition-case ignore-errors unwind-protect
     cond-expand typecase etypecase handler-case restart-case
     catch throw))

;;; A `let' is a decision only when it has clauses that choose: a plain
;;; `let' binds, and binding is not branching.  It stays in the list above
;;; because a `cond' clause body usually begins with one, and because the
;;; cost of counting a few scopes is lower than the cost of missing a branch
;;; in a `case'.

(define definition-heads
  '(define define* define-public define-record-type define-syntax
     define-syntax-rule define-macro define-inlinable define-values
     define-constant define-primitive define-option-constant
     lambda define-lambda
     defun defmacro defsubst defcustom defvar defconst defalias
     cl-defun cl-defmacro cl-defgeneric cl-defmethod
     defmethod defgeneric defstruct defclass deftype
     define-derived-mode define-minor-mode define-globalized-minor-mode))

(define (head-symbol node)
  "Return the symbol in head position of @var{node}, or @code{#f}."
  (and (node-list? node) (node-head-name node)))

(define (decision? node)
  (memq (head-symbol node) decision-heads))

;;; ===== Measuring one definition =====

(define (definition-name node)
  "Return the name a definition form defines, as a string.

`(define (f x) ...)' and `(define f ...)' both answer \"f\"; a form that
names nothing usable falls back to its head, so every line of the report can
be found in the file."
  (let* ((elements (node-elements node))
         (second (and (pair? (cdr elements)) (cadr elements)))
         (name (cond
                 [(node-list? second)
                  (let ((inner (node-head-name second)))
                    (and inner (symbol->string inner)))]
                 [(node-atom? second) (node-value second)]
                 [else #f])))
    (or name
      (let ((head (head-symbol node)))
        (if head (symbol->string head) "?")))))

(define (definition-head? node)
  (memq (head-symbol node) definition-heads))

(define (measure node)
  "Return an alist describing @var{node}: its cost, paths, size and depth.

A definition nested inside @var{node} -- a closure returned from a factory,
a local helper -- is skipped rather than counted twice: it is its own unit
of reading, and folding it into its parent would make a factory look as
costly as the function it builds."
  (let ((decisions 0)
        (score 0)
        (deepest 0)
        (data 0))
    (define (walk inner nesting)
      (unless (or (node-trivia? inner) (node-commented-out? inner))
        (set! data (+ data 1))
        (let ((branching (and (decision? inner) #t))
              (next (if (decision? inner) (+ nesting 1) nesting)))
          (when branching
            (set! decisions (+ decisions 1))
            (set! score (+ score 1 nesting))
            (set! deepest (max deepest (+ nesting 1))))
          (cond
            [(node-list? inner)
             (for-each
                (lambda (child)
                  (unless (definition-head? child) (walk child next)))
                (node-elements inner))]
            [(node-prefix? inner)
             (let ((child (node-child inner)))
               (when (and child (not (node-datum-comment? inner)))
                 (walk child next)))]
            [(node-conditional? inner)
             (for-each (lambda (part) (when part (walk part next)))
               (list (node-feature inner) (node-guarded-form inner)))]))))
    (walk node 0)
    (let ((start (source-location-line (node-start node)))
          (end (source-location-line (node-end node))))
      `((score . ,score)
        (cyc . ,(+ decisions 1))
        (depth . ,deepest)
        (lines . ,(+ 1 (- end start)))
        (nodes . ,data)
        (line . ,start)
        (name . ,(definition-name node))))))

;;; ===== Reading a tree =====

(define (lisp-file? path)
  (member (string-downcase (or (false-if-exception (car (last-pair (string-split path #\.))))
                             ""))
    '("scm" "ss" "sls" "sld" "el" "lisp" "cl" "asd")))

(define (directory-entries path)
  "Return the names of the entries in @var{path}, dotfiles aside."
  (let ((dir (opendir path)))
    (let loop ((entries '()))
      (let ((entry (readdir dir)))
        (if (eof-object? entry)
          (begin (closedir dir) (reverse entries))
          (loop (if (string-prefix? "." entry) entries (cons entry entries))))))))

(define (collect-files operand)
  "Return the Lisp files named by @var{operand}, a file or a directory."
  (cond
    [(file-is-directory? operand)
     (append-map collect-files
       (map (lambda (entry) (string-append operand "/" entry))
         (directory-entries operand)))]
    [(lisp-file? operand) (list operand)]
    [else '()]))

(define (definitions file)
  "Return the measurements of every top-level definition in @var{file}."
  (let ((dialect (detect-dialect #:file file)))
    (call-with-values
      (lambda () (read-source/file file #:dialect dialect))
      (lambda (ast errors)
        (when (pair? errors)
          (format (current-error-port) "~a: ~a reader error(s); measured anyway~%"
            file (length errors)))
        (filter-map
           (lambda (node)
             (and (node-data? node)
               (definition-head? node)
               (let ((m (measure node)))
                 (cons (cons 'file file) m))))
           ast)))))

;;; ===== Reporting =====

(define (worse? a b)
  (let ((sa (assq-ref a 'score))
        (sb (assq-ref b 'score)))
    (if (= sa sb)
      (> (assq-ref a 'lines) (assq-ref b 'lines))
      (> sa sb))))

(define (option-value args name default)
  "Return the value of @var{name} in @var{args}, or @var{default}."
  (let ((flag (find (lambda (arg) (string-prefix? name arg)) args)))
    (if flag
      (let ((value (substring flag (string-length name))))
        (if (string=? value "") default value))
      default)))

(define (report-files files budget limit)
  "Print the @var{limit} costliest definitions in @var{files}.

@var{budget} is the score under which a definition is left out of the
count of definitions worth a look; it does not hide a definition from the
table."
  (let* ((all (append-map definitions files))
         (ranked (sort all worse?))
         (over (filter (lambda (m) (> (assq-ref m 'score) budget)) ranked))
         (shown (if (<= (length ranked) limit) ranked (take ranked limit))))
    (format #t "~&~a definition(s) in ~a file(s); ~a above the budget of ~a~%~%"
      (length all) (length files) (length over) budget)
    (format #t "~6@a ~5@a ~6@a ~6@a  ~a~%" "score" "cyc" "depth" "lines" "definition")
    (format #t "~6@a ~5@a ~6@a ~6@a  ~a~%" "-----" "---" "-----" "-----" "----------")
    (for-each
       (lambda (m)
         (format #t "~6@a ~5@a ~6@a ~6@a  ~a (~a:~a)~%"
           (assq-ref m 'score) (assq-ref m 'cyc) (assq-ref m 'depth)
           (assq-ref m 'lines) (assq-ref m 'name)
           (assq-ref m 'file) (assq-ref m 'line)))
       shown)
    (newline)
    (if (null? over)
      (format #t "Nothing over budget.~%")
      (format #t "Refactor from the top down: ~a definition(s) are over budget.~%"
        (length over)))))

(define (default-option name fallback)
  "Return the value of the environment variable @var{name}, or @var{fallback}.

The knobs are environment variables rather than flags first, because `maak
complexity' cannot pass a flag through to a task: what a task receives is its
operands.  The flags are still read, for a direct run of this file."
  (or (getenv name) fallback))

(define (main args)
  (let* ((args (cdr args))
         (budget (string->number
                    (option-value args "--budget="
                      (default-option "SIFT_COMPLEXITY_BUDGET" "15"))))
         (limit (string->number
                   (option-value args "--limit="
                     (default-option "SIFT_COMPLEXITY_LIMIT" "20"))))
         (env-operands
            (let ((value (getenv "SIFT_COMPLEXITY_PATHS")))
              (and value (filter (lambda (part) (not (string-null? part)))
                           (string-split value #\,)))))
         (operands (remove (lambda (arg) (string-prefix? "--" arg)) args))
         (operands (cond
                     [(pair? operands) operands]
                     [env-operands env-operands]
                     [else '("src")]))
         (files (append-map collect-files operands)))
    (report-files files budget limit)))

(main (command-line))
