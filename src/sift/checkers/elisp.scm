;; sift --- Emacs Lisp checkers
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Emacs Lisp is the dialect with the most to say about how code is written
;; down.  The Emacs Lisp Reference Manual asks every command, function and
;; variable intended for users to carry a docstring, asks the first line of
;; that docstring to be a complete sentence that fits in 74 columns for
;; `apropos', asks continuation lines to start at column zero, and asks the
;; first sentence to be in the imperative -- "Return ...", not "Returns ...".
;; These checkers report what the manual asks for, each one separately, so a
;; project can take the ones it wants.

(define-module (sift checkers elisp)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift check base)
  #:export (elisp-checkers
              docstring-missing-checker
              docstring-first-line-checker
              docstring-indent-checker
              lexical-binding-missing-checker
              interactive-without-docstring-checker
              checkdoc-style-verb-checker
              provide-missing-checker
              declare-indent-checker
              docstring-arguments-checker))

;;; ===== helpers =====

(define documented-heads
  '(defun defmacro defsubst defvar defcustom))

(define (docstring-text node)
  "Return the text of a docstring node, without its quotes."
  (let ((value (node-value node)))
    (if (and (>= (string-length value) 2)
          (char=? (string-ref value 0) #\")
          (char=? (string-ref value (- (string-length value) 1)) #\"))
      (substring value 1 (- (string-length value) 1))
      value)))

(define (docstring-first-line text)
  (let ((newline (string-index text #\newline)))
    (if newline (substring text 0 newline) text)))

;;; ===== docstring-missing =====

(define (docstring-missing-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
       (lambda (node)
         (let ((head (and (node-list? node) (node-head-name node))))
           (when (and head
                   (memq head documented-heads)
                   (not (docstring-node node))
                   ;; a bare (defvar x) declares without defining, and needs
                   ;; no docstring
                   (>= (argument-count node) 2))
             (set! warnings
               (cons (make-lint-warning
                        'docstring-missing 'warning
                        (format #f "`~a` form has no docstring" head)
                        (node-start node))
                 warnings)))))
       ast)
    (reverse warnings)))

(define docstring-missing-checker
  (checker 'docstring-missing
    "`defun`, `defmacro`, `defsubst`, `defvar` or `defcustom` without a docstring"
    '(elisp)
    docstring-missing-warnings
    #:severity 'warning
    #:example '((input . "(defun f (x) (+ x 1))")
                (fix . "no automatic fix; write the docstring"))))

;;; ===== docstring-first-line =====

(define (docstring-first-line-warnings ast source dialect)
  (let ((warnings '()))
    (define (report node message)
      (set! warnings
        (cons (make-lint-warning 'docstring-first-line 'warning message
                (node-start node))
          warnings)))
    (for-each-node
       (lambda (node)
         (when (node-list? node)
           (let ((doc (docstring-node node)))
             (when doc
               (let* ((text (docstring-text doc))
                      (first (docstring-first-line text)))
                 (unless (string-null? first)
                   (when (> (string-length first) 74)
                     (report doc
                       (format #f "the first line of the docstring is ~a columns long, over the 74 the manual recommends"
                         (string-length first))))
                   (let ((initial (car (string->list first))))
                     (when (and initial (char-lower-case? initial))
                       (report doc "the first line of the docstring does not start with a capital letter")))
                   (let ((last (car (reverse (string->list (string-trim-right first))))))
                     (when (and last
                             (not (memv last '(#\. #\? #\! #\:))))
                       (report doc "the first line of the docstring does not end with a period")))))))))
       ast)
    (reverse warnings)))

(define docstring-first-line-checker
  (checker 'docstring-first-line
    "The first line of a docstring is not a capitalised sentence ending in a period"
    '(elisp)
    docstring-first-line-warnings
    #:severity 'warning
    #:example '((input . "(defun f (x)\n  \"returns x.\"\n  x)")
                (fix . "no automatic fix; rewrite the first line"))))

;;; ===== docstring-indent =====

(define (docstring-indent-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
       (lambda (node)
         (when (node-list? node)
           (let ((doc (docstring-node node)))
             (when doc
               (let* ((text (docstring-text doc))
                      (lines (cdr (string-split text #\newline))))
                 (when (any (lambda (line)
                              (and (not (string-null? line))
                                (memv (string-ref line 0) '(#\space #\tab))))
                         lines)
                   (set! warnings
                     (cons (make-lint-warning
                              'docstring-indent 'warning
                              "docstring continuation lines are indented; start them at column zero"
                              (node-start doc))
                       warnings))))))))
       ast)
    (reverse warnings)))

(define docstring-indent-checker
  (checker 'docstring-indent
    "Docstring continuation lines are indented instead of starting at column zero"
    '(elisp)
    docstring-indent-warnings
    #:severity 'warning
    #:example '((input . "(defun f (x)\n  \"Doc.\n    More.\"\n  x)")
                (fix . "no automatic fix; unindent the continuation lines"))))

;;; ===== lexical-binding-missing =====

(define (first-lines source n)
  (take (string-split source #\newline) (min n (length (string-split source #\newline)))))

(define (lexical-binding-present? source)
  (any (lambda (line) (string-contains line "lexical-binding: t"))
    (first-lines source 2)))

(define (lexical-binding-missing-warnings ast source dialect)
  (if (lexical-binding-present? source)
    '()
    (list (make-lint-warning/range
             'lexical-binding-missing 'warning
             "the file does not declare `lexical-binding: t'"
             (make-source-location #f 1 1 0 0)
             0 0
             (lambda (source start end)
               ";;; -*- lexical-binding: t; -*-\n")))))

(define lexical-binding-missing-checker
  (checker 'lexical-binding-missing
    "The file header lacks `-*- lexical-binding: t; -*-`"
    '(elisp)
    lexical-binding-missing-warnings
    #:severity 'warning
    #:example '((input . ";;; foo.el --- a package\n")
                (fix . "insert \"-*- lexical-binding: t; -*-\" in the file header"))))

;;; ===== interactive-without-docstring =====

(define (interactive-without-docstring-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
       (lambda (node)
         (let ((head (and (node-list? node) (node-head-name node))))
           (when (and head (memq head '(defun defmacro)))
             (let ((interactive (find (lambda (element)
                                        (list-headed-by? element 'interactive))
                                  (body-elements node 3))))
               (when (and interactive (not (docstring-node node)))
                 (set! warnings
                   (cons (make-lint-warning
                            'interactive-without-docstring 'warning
                            "an interactive command without a docstring"
                            (node-start interactive))
                     warnings)))))))
       ast)
    (reverse warnings)))

(define interactive-without-docstring-checker
  (checker 'interactive-without-docstring
    "An `interactive` command without a docstring"
    '(elisp)
    interactive-without-docstring-warnings
    #:severity 'warning
    #:example '((input . "(defun my-command ()\n  (interactive)\n  (message \"hi\"))")
                (fix . "no automatic fix; document the command"))))

;;; ===== checkdoc-style-verb =====

(define third-person-verbs
  '("Returns" "Calls" "Creates" "Sets" "Gets" "Checks" "Adds" "Removes"
    "Starts" "Stops" "Opens" "Closes" "Kills" "Sends" "Loads" "Builds"
    "Computes" "Converts" "Displays" "Draws" "Finds" "Formats" "Makes"
    "Moves" "Parses" "Prints" "Reads" "Runs" "Shows" "Sorts" "Toggles"
    "Updates" "Writes"))

(define (checkdoc-style-verb-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
       (lambda (node)
         (when (node-list? node)
           (let ((doc (docstring-node node)))
             (when doc
               (let* ((first (docstring-first-line (docstring-text doc)))
                      (word (car (string-split first #\space))))
                 (when (and word (member word third-person-verbs))
                   (set! warnings
                     (cons (make-lint-warning
                              'checkdoc-style-verb 'info
                              (format #f "the docstring opens with `~a`; the manual asks for the imperative"
                                word)
                              (node-start doc))
                       warnings))))))))
       ast)
    (reverse warnings)))

(define checkdoc-style-verb-checker
  (checker 'checkdoc-style-verb
    "Docstring first sentence uses the third person instead of the imperative"
    '(elisp)
    checkdoc-style-verb-warnings
    #:severity 'info
    #:example '((input . "(defun f (x)\n  \"Returns x.\"\n  x)")
                (fix . "no automatic fix; write \"Return x.\""))))

;;; ===== provide-missing =====

(define (defines-something? ast)
  (let ((found #f))
    (for-each-top-level
       (lambda (node)
         (let ((head (and (node-list? node) (node-head-name node))))
           (when (and head (memq head '(defun defmacro defsubst defvar
                                         defcustom defface define-minor-mode)))
             (set! found #t))))
       ast)
    found))

(define (provide-missing-warnings ast source dialect)
  (if (or (not (defines-something? ast))
        (any (lambda (node) (list-headed-by? node 'provide))
          (filter node-data? ast)))
    '()
    (let* ((last-node (car (reverse (filter node-data? ast))))
           (index (if last-node (node-end-index last-node) 0))
           (line (if last-node (source-location-line (node-end last-node)) 1)))
      (list (make-lint-warning
               'provide-missing 'warning
               "the file defines a package but never calls `provide`"
               (make-source-location #f line 1 index index))))))

(define provide-missing-checker
  (checker 'provide-missing
    "The file defines a package but has no final `(provide 'feature)`"
    '(elisp)
    provide-missing-warnings
    #:severity 'warning
    #:example '((input . "(defun my-f ()\n  \"Do it.\"\n  nil)")
                (fix . "no automatic fix; add (provide 'my-feature) at the end"))))

;;; ===== declare-indent =====

(define (declare-indent-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
       (lambda (node)
         (when (and (node-list? node)
                 (head-is-one-of? node '(defun defmacro defsubst
                                          cl-defun cl-defmacro)))
           (let ((open-column (node-column node)))
             (for-each
                (lambda (element)
                  (when (list-headed-by? element 'declare)
                    ;; node-column is 1-based, so the body's column is the
                    ;; opener's column plus two, and it starts that many
                    ;; characters in from the beginning of the line
                    (let ((expected (+ open-column 2)))
                      (when (not (= (node-column element) expected))
                        (let* ((previous (previous-element node element))
                               (start (if previous (node-end-index previous)
                                        (node-index element))))
                          (set! warnings
                            (cons (make-lint-warning/range
                                     'declare-indent 'info
                                     (format #f "`declare` should be indented ~a columns from `~a`"
                                       (- expected open-column)
                                       (node-head-name node))
                                     (node-start element)
                                     start (node-index element)
                                     (lambda (source s e)
                                       (string-append "\n"
                                         (make-string (- expected 1)
                                           #\space))))
                              warnings)))))))
                (body-elements node 3)))))
       ast)
    (reverse warnings)))

(define (previous-element node element)
  "Return the element of @var{node} before @var{element}, or @code{#f}."
  (let loop ((rest (node-elements node)) (previous #f))
    (cond [(null? rest) #f]
          [(eq? (car rest) element) previous]
          [else (loop (cdr rest) (car rest))])))

(define declare-indent-checker
  (checker 'declare-indent
    "`declare` not indented two columns from the enclosing `defun`"
    '(elisp)
    declare-indent-warnings
    #:severity 'info
    #:example '((input . "(defun f (x)\n    (declare (indent 1))\n  x)")
                (fix . "indent the `declare` two columns from the `defun`"))))

;;; ===== docstring-arguments =====

(define (parameter-names node)
  "Return the parameter names in the argument list of @var{node}.

Only plain names count: a parameter written as @code{(name default)} for an
optional argument contributes @code{name}, and a marker such as
@code{&optional} contributes nothing."
  ;; the first argument of a `defun` is the name; the second is the list of
  ;; parameters
  (let* ((arguments (form-arguments node))
         (formals (and (pair? arguments) (pair? (cdr arguments))
                    (cadr arguments))))
    (if (not (node-list? formals))
      '()
      (append-map
         (lambda (element)
           (cond
             [(node-atom? element)
              (let ((value (node-value element)))
                (if (or (member value '("&optional" "&rest" "&key" "&aux"
                                        "&allow-other-keys" "&body"))
                      (not value))
                  '()
                  (list value)))]
             [(node-list? element)
              (let ((head (node-head element)))
                (if (and head (node-atom? head)
                      (eq? (node-atom-type head) 'symbol))
                  (list (node-value head))
                  '()))]
             [else '()]))
         (node-elements formals)))))

(define (docstring-words text)
  "Return the words of a docstring, lower-cased.

Matching a parameter name as a substring would accept \"Add two numbers.\"
as mentioning the argument `a`; matching it as a word is what checkdoc
does, and what a reader does."
  (map string-downcase
    (remove string-null?
      (string-split text
        (char-set-complement
           (char-set #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l
             #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
             #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
             #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
             #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\_))))))

(define (docstring-arguments-warnings ast source dialect)
  (let ((warnings '()))
    (for-each-node
       (lambda (node)
         (let ((head (and (node-list? node) (node-head-name node))))
           (when (and head (memq head '(defun defmacro defsubst cl-defun
                                         cl-defmacro cl-defsubst)))
             (let ((doc (docstring-node node)))
               (when doc
                 (let ((words (docstring-words (node-value doc))))
                   (for-each
                      (lambda (parameter)
                        (unless (member (string-downcase parameter) words)
                          (set! warnings
                            (cons (make-lint-warning 'docstring-arguments 'info
                                    (format #f "the docstring does not mention the argument `~a`; Emacs shows it in `C-h f`"
                                      parameter)
                                    (node-start doc))
                              warnings))))
                      (parameter-names node))))))))
       ast)
    (reverse warnings)))

(define docstring-arguments-checker
  (checker 'docstring-arguments
    "An Emacs Lisp docstring that does not mention one of its arguments"
    '(elisp)
    docstring-arguments-warnings
    #:severity 'info
    #:example '((input . "(defun add (a b)\n  \"Add two numbers.\"\n  (+ a b))")
                (fix . "(defun add (a b)\n  \"Add A and B.\"\n  (+ a b))"))))

;;; ===== The list =====

(define elisp-checkers
  (list docstring-missing-checker
    docstring-first-line-checker
    docstring-indent-checker
    lexical-binding-missing-checker
    interactive-without-docstring-checker
    checkdoc-style-verb-checker
    provide-missing-checker
    declare-indent-checker
    docstring-arguments-checker))
