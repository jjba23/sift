;; sift --- whitespace checkers
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

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
              trailing-blank-lines-checker))

;;; ===== trailing-whitespace =====

(define (trailing-whitespace-warnings ast source dialect)
  (let ((spans (line-spans source))
        (warnings '()))
    (let loop ((rest spans) (number 1))
      (unless (null? rest)
        (let* ((span (car rest))
               (text (substring source (car span) (cdr span)))
               (trimmed (string-trim-right text (char-set #\space #\tab))))
          (when (< (string-length trimmed) (string-length text))
            (let* ((first-bad (+ (car span) (string-length trimmed))))
              (set! warnings
                (cons (make-lint-warning/range
                         'trailing-whitespace 'warning
                         "line ends with whitespace"
                         (location-at-index source first-bad)
                         first-bad (cdr span)
                         (lambda (source start end) ""))
                  warnings)))))
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
    (cond ((>= i end) end)
      ((memv (string-ref source i) '(#\space #\tab)) (loop (+ i 1)))
      (else i))))

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
               (indent-end (leading-whitespace-end source start end))
               (indent (substring source start indent-end)))
          (when (and (< (string-length indent) (- end start))
                  ;; a line that begins inside a string or a block comment
                  ;; is text, not code
                  (not (index-in-ranges? start exempt))
                  (string-index indent #\tab))
            (let* ((tab-at (+ start (string-index indent #\tab)))
                   (expanded
                      (string-join
                         (map (lambda (c)
                                (if (char=? c #\tab) (make-string width #\space) " "))
                           (string->list indent))
                         "")))
              (set! warnings
                (cons (make-lint-warning/range
                         'tabs 'warning
                         "indentation contains a tab character"
                         (location-at-index source tab-at)
                         start indent-end
                         (lambda (source s e) expanded))
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
                  (if (eq? dialect 'elisp) 80 100)))
         (spans (line-spans source))
         (warnings '()))
    (let loop ((rest spans) (number 1))
      (unless (null? rest)
        (let ((span (car rest)))
          (when (> (string-length (substring source (car span) (cdr span)))
                  limit)
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
    "Line exceeds the configured soft limit"
    'all
    line-length-warnings
    #:severity 'warning
    #:example '((input . "(define (f x) (some-very-long-call ...))  ; over 100 columns")
                (fix . "no automatic fix; break the form by hand"))))

;;; ===== trailing-blank-lines =====

(define (trailing-blank-lines-warnings ast source dialect)
  (let* ((spans (line-spans source))
         (ends-with-newline?
            (let ((n (string-length source)))
              (and (> n 0) (char=? (string-ref source (- n 1)) #\newline))))
         (last-non-blank
            (let loop ((rest spans) (number 1) (found 0))
              (cond ((null? rest) found)
                ((blank-line? (substring source (caar rest) (cdar rest)))
                 (loop (cdr rest) (+ number 1) found))
                (else (loop (cdr rest) (+ number 1) number)))))
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

;;; ===== The list =====

(define whitespace-checkers
  (list trailing-whitespace-checker
    tabs-checker
    line-length-checker
    trailing-blank-lines-checker))
