#!/usr/bin/env guile
!#
;; sift --- complexity report
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
;; report, not a gate.  The gate is the linter: `cognitive-complexity' and
;; `cyclomatic-complexity' compare a definition against the same budget and
;; `sift ci' fails a run that is over it.

(use-modules (ice-9 format)
  (srfi srfi-1)
  (srfi srfi-13)
  (sift complexity)
  (sift reader)
  (sift records))

;;; ===== Reading a definition =====

;;; What a decision is, and what one costs, is `(sift complexity)'s answer:
;;; the two rules `cognitive-complexity' and `cyclomatic-complexity' ask the
;;; same question, and a report that measured it differently would send its
;;; reader to refactor something the linter says is fine.

(define (row file node)
  "Return one line of the report for @var{node}, measured in @var{file}."
  (let ((measure (complexity-of node)))
    (list (cons 'file file)
      (cons 'score (complexity-score measure))
      (cons 'cyc (complexity-cycles measure))
      (cons 'depth (complexity-depth measure))
      (cons 'lines (complexity-lines measure))
      (cons 'line (complexity-line measure))
      (cons 'name (complexity-name measure)))))

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
               (complexity-definition? node)
               (row file node)))
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
