;; sift --- parenthesis checkers
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Three checkers about delimiters: one that reports a file whose brackets do
;; not balance, one that reports the dangling style in Common Lisp, and one
;; that reports a reader prefix separated from the datum it applies to.
;;
;; The first never guesses.  A missing `)' is reported where the form was
;; opened, not where sift would like it to be, because an editor that inserts
;; a delimiter on a guess can silently change what a program means.

(define-module (sift checkers parens)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift check base)
  #:export (parens-checkers
              unmatched-parens-checker
              dangling-paren-checker
              reader-prefix-break-checker))

;;; ===== unmatched-parens =====

(define (open-token-name token)
  (cond ((equal? token "(") "parenthesis")
    ((equal? token "[") "bracket")
    ((equal? token "{") "brace")
    (else "delimiter")))

(define (unmatched-parens-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
       (lambda (node)
         (cond
           ((and (node-list? node) (not (node-close node)))
            (set! warnings
              (cons (make-lint-warning
                       'unmatched-parens 'error
                       (format #f "unmatched opening ~a"
                         (open-token-name (node-open node)))
                       (node-start node))
                warnings)))
           ((node-stray-delimiter? node)
            (set! warnings
              (cons (make-lint-warning
                       'unmatched-parens 'error
                       (format #f "unmatched closing delimiter `~a'"
                         (node-value node))
                       (node-start node))
                warnings)))))
       ast)
    (reverse warnings)))

(define unmatched-parens-checker
  (checker 'unmatched-parens
    "Unbalanced or unmatched parentheses or brackets"
    'all
    unmatched-parens-warnings
    #:severity 'error
    #:example '((input . "(define (f x)\n  x")
                (fix . "no automatic fix; sift never guesses where a `)' goes"))))

;;; ===== dangling-paren =====

(define (last-written-child children)
  (let loop ((rest (reverse children)))
    (cond ((null? rest) #f)
      ((node-whitespace? (car rest)) (loop (cdr rest)))
      (else (car rest)))))

(define (close-on-own-line? node)
  "Return @code{#t} when the closer of @var{node} starts a line of its own."
  (let ((children (node-children node)))
    (and (node-close node)
      (pair? children)
      (let loop ((rest (reverse children)))
        (cond ((null? rest) #f)
          ((node-whitespace? (car rest))
           (if (> (node-newlines (car rest)) 0)
             #t
             (loop (cdr rest))))
          (else #f))))))

(define (dangling-paren-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
       (lambda (node)
         (when (and (node-list? node) (close-on-own-line? node))
           (let* ((close (node-close node))
                  (close-index (- (node-end-index node) (string-length close)))
                  (last (last-written-child (node-children node)))
                  (range (and last
                           (not (node-comment? last))
                           (cons (node-end-index last) (node-end-index node)))))
             (set! warnings
               (cons (make-lint-warning/range
                        'dangling-paren 'warning
                        "closing parenthesis is on its own line"
                        (location-at-index source close-index)
                        (if range (car range) close-index)
                        (if range (cdr range) (node-end-index node))
                        (and range (lambda (source start end) close)))
                 warnings)))))
       ast)
    (reverse warnings)))

(define dangling-paren-checker
  (checker 'dangling-paren
    "Closing parenthesis on its own line"
    '(cl)
    dangling-paren-warnings
    #:severity 'warning
    #:example '((input . "(defun f (x)\n  x\n  )")
                (fix . "pull the closing parenthesis onto the previous line"))))

;;; ===== reader-prefix-break =====

(define (reader-prefix-break-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
       (lambda (node)
         (when (node-prefix? node)
           (let* ((token (node-token node))
                  (child (node-child node))
                  (gap-start (+ (node-index node) (string-length token))))
             (when (and child
                     (string-index (substring source gap-start
                                     (node-index child))
                       #\newline))
               (set! warnings
                 (cons (make-lint-warning/range
                          'reader-prefix-break 'warning
                          (format #f "~a is separated from its datum by a newline"
                            token)
                          (node-start node)
                          gap-start (node-index child)
                          (lambda (source start end) ""))
                   warnings))))))
       ast)
    (reverse warnings)))

(define reader-prefix-break-checker
  (checker 'reader-prefix-break
    "A newline between a reader prefix and its datum"
    'all
    reader-prefix-break-warnings
    #:severity 'warning
    #:example '((input . "'\n(foo)")
                (fix . "join the prefix to the datum it applies to"))))

;;; ===== The list =====

(define parens-checkers
  (list unmatched-parens-checker
    dangling-paren-checker
    reader-prefix-break-checker))
