;; sift --- whitespace checkers
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The four checkers that look at the shape of a line rather than at the
;; shape of a form: trailing whitespace, tabs used for indentation, lines
;; over the soft limit, and blank lines piled up at the end of a file.
;;
;; Everything here works on the source text directly, because that is what
;; the question is about.  A tab inside a string is data and a line that
;; begins inside a block comment is not indentation, so both are excluded
;; using the ranges the reader recorded for those nodes.

(define-module (sift checkers whitespace)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift check base)
  #:export (whitespace-checkers
            trailing-whitespace-checker
            tabs-checker
            line-length-checker
            trailing-blank-lines-checker
            blank-line-in-body-checker))

;;; ===== trailing-whitespace =====

(define (trailing-whitespace-at source start end)
  "Return the index where the whitespace that ends a line begins, or @code{#f}.

The line is scanned from its end rather than copied out of the source: a
checker that allocates a string per line of a two-thousand-line file spends
most of its time in the garbage collector."
  (let loop ((i end))
    (if (and (> i start)
          (memv (string-ref source (- i 1)) '(#\space #\tab)))
      (loop (- i 1))
      (and (< i end) i))))

(define (trailing-whitespace-warnings ast source dialect)
  (let ((spans (line-spans source))
        (warnings '()))
    (let loop ((rest spans) (number 1))
      (unless (null? rest)
        (let* ((span (car rest))
               (first-bad (trailing-whitespace-at source (car span) (cdr span))))
          (when first-bad
            (set! warnings
                  (cons (make-lint-warning/range
                         'trailing-whitespace 'warning
                         "line ends with whitespace"
                         (location-at-index source first-bad)
                         first-bad (cdr span)
                         (lambda (source start end) ""))
                        warnings))))
        (loop (cdr rest) (+ number 1))))
    (reverse warnings)))

(define trailing-whitespace-checker
  (checker 'trailing-whitespace
           "Lines ending with spaces or tabs"
           'all
           trailing-whitespace-warnings
           #:severity 'warning
           #:example '((input . "(define (f x) \n  x)")
                       (fix . "strip the trailing whitespace"))))

;;; ===== tabs =====

(define (leading-whitespace-end source start end)
  "Return the index of the first character of a line that is not indentation."
  (let loop ((i start))
    (cond [(>= i end) end]
          [(memv (string-ref source i) '(#\space #\tab)) (loop (+ i 1))]
          [else i])))

(define (indent-tab-at source start end)
  "Return the index of the first tab in the indentation of a line, or @code{#f}.

The indentation is scanned in place: a substring per line is an allocation per
line, and a file of two thousand lines has two thousand lines."
  (let loop ((i start))
    (cond
      [(>= i end) #f]
      [(char=? (string-ref source i) #\tab) i]
      [(char=? (string-ref source i) #\space) (loop (+ i 1))]
      [else #f])))

(define (expand-indentation source start end width)
  "Return the indentation from @var{start} to @var{end} with tabs expanded."
  (let loop ((i start) (out '()))
    (if (>= i end)
      (list->string (reverse! out))
      (if (char=? (string-ref source i) #\tab)
        (loop (+ i 1)
              (let pad ((n width) (acc out))
                (if (zero? n) acc (pad (- n 1) (cons #\space acc)))))
        (loop (+ i 1) (cons #\space out))))))

(define (tabs-warnings ast source dialect)
  (let* ((spans (line-spans source))
         (exempt (text-ranges-of ast))
         (width (option 'indent 2))
         (warnings '()))
    (let loop ((rest spans) (number 1))
      (unless (null? rest)
        (let* ((span (car rest))
               (start (car span))
               (end (cdr span))
               (tab-at (indent-tab-at source start end)))
          ;; A line that begins inside a string or a block comment is text,
          ;; not code -- and the ranges are only asked about a line that has a
          ;; tab in its indentation, which is the rare one.
          (when (and tab-at (not (index-in-ranges? start exempt)))
            (let ((indent-end (leading-whitespace-end source start end)))
              (set! warnings
                    (cons (make-lint-warning/range
                           'tabs 'warning
                           "indentation contains a tab character"
                           (location-at-index source tab-at)
                           start indent-end
                           (lambda (source s e)
                             (expand-indentation source s e width)))
                          warnings))))
          (loop (cdr rest) (+ number 1))))
      (reverse warnings))))

(define tabs-checker
  (checker 'tabs
           "Tab characters used for indentation"
           'all
           tabs-warnings
           #:severity 'warning
           #:example '((input . "(define (f x)\n\tx)")
                       (fix . "replace the tab with spaces at the indent width"))))

;;; ===== line-length =====

(define (line-length-warnings ast source dialect)
  (let* ((limit (or (option 'line-length #f)
                  (if (eq? dialect 'elisp) 80 96)))
         (spans (line-spans source))
         (warnings '()))
    (let loop ((rest spans) (number 1))
      (unless (null? rest)
        (let ((span (car rest)))
          ;; the length of a line is the distance between its two ends: asking
          ;; for the substring of every line to measure it is a copy per line
          (when (> (- (cdr span) (car span)) limit)
            (set! warnings
                  (cons (make-lint-warning 'line-length 'warning
                                           (format #f "line is ~a columns long, over the limit of ~a"
                                                   (- (cdr span) (car span))
                                                   limit)
                                           (make-source-location #f number
                                                                 (+ limit 1)
                                                                 (+ (car span) limit)
                                                                 (+ (car span) limit)))
                        warnings))))
        (loop (cdr rest) (+ number 1))))
    (reverse warnings)))

(define line-length-checker
  (checker 'line-length
           "Line exceeds the configured soft limit (96 columns, 80 in Emacs Lisp)"
           'all
           line-length-warnings
           #:severity 'warning
           #:example '((input . "(define (f x) (some-very-long-call ...))  ; over 96 columns")
                       (fix . "no automatic fix; break the form by hand"))))

;;; ===== trailing-blank-lines =====

(define (trailing-blank-lines-warnings ast source dialect)
  (let* ((spans (line-spans source))
         (ends-with-newline?
          (let ((n (string-length source)))
            (and (> n 0) (char=? (string-ref source (- n 1)) #\newline))))
         (last-non-blank
          (let loop ((rest spans) (number 1) (found 0))
            (cond [(null? rest) found]
                  [(blank-line? source (caar rest) (cdar rest))
                   (loop (cdr rest) (+ number 1) found)]
                  [else (loop (cdr rest) (+ number 1) number)])))
         ;; The final line a trailing newline creates is not a blank line the
         ;; author wrote, so it is not counted.
         (trailing-blank (- (length spans) last-non-blank
                            (if ends-with-newline? 1 0))))
    (if (> trailing-blank 2)
      (let ((start (if (> last-non-blank 0)
                     (cdr (list-ref spans (- last-non-blank 1)))
                     0)))
        (list (make-lint-warning/range
               'trailing-blank-lines 'info
               (format #f "~a blank lines at end of file" trailing-blank)
               (make-source-location #f (+ last-non-blank 1) 1 start start)
               start (string-length source)
               (lambda (source s e) "\n\n"))))
      '())))

(define trailing-blank-lines-checker
  (checker 'trailing-blank-lines
           "More than two consecutive blank lines at end of file"
           'all
           trailing-blank-lines-warnings
           #:severity 'info
           #:example '((input . "(define x 1)\n\n\n\n\n")
                       (fix . "trim the end of the file to one blank line"))))

;;; ===== blank-line-in-body =====
;;
;; A blank line is a paragraph break, and the body of one form is one
;; paragraph.  Riastradh's rule allows blank lines in a body only to separate
;; internal definitions -- where there really are two paragraphs -- so this
;; rule counts the gaps between the forms of a body and accepts a gap when
;; the definition on either side of it is a definition of its own.

;; The forms whose body is a sequence of expressions, and how many elements
;; come before that body: `(define (f x) body ...)' has two, `(lambda (x)
;; body ...)' has two, `(defun f (x) body ...)' has three.
(define body-form-skips
  '((define . 2) (define* . 2) (define-public . 2) (define-inlinable . 2)
    (lambda . 2) (lambda* . 2) (case-lambda . 2)
    (defun . 3) (defmacro . 3) (defsubst . 3) (cl-defun . 3)
    (cl-defmacro . 3) (cl-defsubst . 3) (defmethod . 3) (defgeneric . 3)
    (let . 2) (let* . 2) (letrec . 2) (letrec* . 2)))

(define (internal-definition? node)
  "Return @code{#t} when @var{node} is a definition inside a body.

A blank line between two of these is the one a body is allowed to have: the
rule exists so that a reader can see where one internal definition stops and
the next starts."
  (memq (and (node-list? node) (node-head-name node))
        '(define define* define-public define-inlinable define-syntax
           define-record-type define-class defun defmacro defsubst
           defvar defconst defcustom defparameter defconstant)))

(define (blank-line-in-body-warnings ast source dialect)
  (let ((warnings '()))
    (define (report element)
      (set! warnings
            (cons (make-lint-warning
                   'blank-line-in-body 'info
                   "blank line inside a body; the body of a form is one paragraph"
                   (node-start element))
                  warnings)))
    (for-each-node
     (lambda (node)
       (let* ((head (and (node-list? node) (node-head-name node)))
              (skip (and head (assq-ref body-form-skips head))))
         (when skip
           (let loop ((rest (node-children node))
                      (index -1)
                      (previous #f)
                      (blank? #f))
             (unless (null? rest)
               (let ((child (car rest)))
                 (cond
                   [(node-whitespace? child)
                    (loop (cdr rest) index previous
                          (or blank? (>= (node-newlines child) 2)))]
                   ;; A comment starts the next paragraph, and a comment above a
                   ;; form is that form's heading: a blank line before it is
                   ;; the author's, and is not a gap in the body.
                   [(node-comment? child) (loop (cdr rest) index previous #f)]
                   [(node-data? child)
                    (let ((next (+ index 1)))
                      (when (and blank? previous
                              (>= index skip)
                              (>= next skip)
                              (not (internal-definition? previous))
                              (not (internal-definition? child)))
                        (report child))
                      (loop (cdr rest) next child #f))]
                   [else (loop (cdr rest) index previous blank?)])))))))
     ast)
    (reverse warnings)))

(define blank-line-in-body-checker
  (checker 'blank-line-in-body
           "A blank line in the middle of a body, where one paragraph belongs"
           'all
           blank-line-in-body-warnings
           #:severity 'info
           #:example '((input . "(define (f x)
  (first x)

  (second x))
")
                       (fix . "delete the blank line, or split the form in two"))))

;;; ===== The list =====

(define whitespace-checkers
  (list trailing-whitespace-checker
        tabs-checker
        line-length-checker
        trailing-blank-lines-checker
        blank-line-in-body-checker))
