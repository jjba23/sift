;; sift --- lint driver
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; The driver reads a file once, runs the applicable checkers over the AST,
;; folds in what the reader found, applies the suppression comments, sorts
;; the result and -- only when asked -- applies the fixes.
;;
;; Two rules from the manual are enforced here rather than trusted to each
;; checker.  Nothing is reported about a datum comment or inside a form the
;; reader marked opaque, because the walk the checkers use does not enter
;; them.  And no fix is applied unless the source still reads afterwards: a
;; fix that introduces a parse error is discarded, along with every other fix
;; of the same run, and the original warnings are reported unfixed.

(define-module (sift lint)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift check base)
  #:use-module (sift checkers whitespace)
  #:use-module (sift checkers parens)
  #:use-module (sift checkers bodies)
  #:use-module (sift checkers scheme)
  #:use-module (sift checkers elisp)
  #:use-module (sift checkers cl)
  #:use-module (sift checkers ergonomics)
  #:use-module (sift checkers naming)
  #:export (all-checkers
              find-checker
              checkers-for-dialect
              checkers-by-name
              lint-source
              lint-string
              lint-file
              lint-files
              apply-fixes
              suppression-directives
              suppressed?
              checkers-applicable?
              severity-at-least?
              normalise-checker-name))

;;; ===== Registry =====

(define (all-checkers)
  "Return every checker sift ships, sorted by name.

The order is sorted rather than written down, so `list-checkers' and any
other report over the whole set cannot drift from the set itself."
  (sort (append whitespace-checkers
          parens-checkers
          bodies-checkers
          scheme-checkers
          elisp-checkers
          cl-checkers
          ergonomics-checkers
          naming-checkers)
    (lambda (a b)
      (string<? (symbol->string (lint-checker-name a))
        (symbol->string (lint-checker-name b))))))

(define (normalise-checker-name name)
  "Return @var{name} as a symbol, whatever way it was spelled on the command line."
  (cond [(symbol? name) name]
        [(string? name) (string->symbol name)]
        [else (string->symbol (format #f "~a" name))]))

(define* (find-checker name)
  "Return the checker named @var{name}, or @code{#f}."
  (let ((wanted (normalise-checker-name name)))
    (find (lambda (checker) (eq? (lint-checker-name checker) wanted))
      (all-checkers))))

(define* (checkers-by-name names)
  "Return the checkers named by @var{names}; unknown names are dropped."
  (filter-map find-checker names))

(define (checkers-for-dialect dialect)
  "Return the checkers that apply to @var{dialect}."
  (filter (lambda (checker) (lint-checker-for-dialect? checker dialect))
    (all-checkers)))

(define* (checkers-applicable? dialect #:key (include #f) (exclude '()))
  "Return the checkers to run for @var{dialect}.

@var{include}, when given, is the list of names to run; @var{exclude} is a
list of names to skip.  Selecting a checker that does not apply to the
dialect is not an error -- it simply runs nothing for that dialect."
  (let* ((available (checkers-for-dialect dialect))
         (selected (if include
                     (filter (lambda (checker)
                               (memq (lint-checker-name checker) include))
                       (all-checkers))
                     available))
         (excluded (map normalise-checker-name (or exclude '()))))
    (remove (lambda (checker)
              (memq (lint-checker-name checker) excluded))
      selected)))

;;; ===== Suppression comments =====

(define directive-prefix "sift:")

(define (parse-directive text)
  "Return @code{(kind . checkers)} for a `sift:` comment, or @code{#f}.

The three forms are the ones the manual documents:

  ;; sift:disable-next-line checker-name
  ;; sift:disable checker-name
  ;; sift:enable checker-name

No checker name means every checker.  Suppression is deliberately
line-oriented and form-scoped: there is no range syntax, because a range
comment in a source file outlives the code it was written for."
  (let* ((trimmed (string-trim (string-trim-both text)
                    (char-set #\; #\space #\tab)))
         (space (string-index trimmed #\space)))
    (and (>= (string-length trimmed) (string-length directive-prefix))
      (string-prefix? directive-prefix trimmed)
      (let* ((directive (if space (substring trimmed 0 space) trimmed))
             (rest (if space
                     (string-trim-both (substring trimmed (+ space 1)))
                     ""))
             (names (filter (lambda (s) (not (string-null? s)))
                      (append-map (lambda (part)
                                    (string-split part #\,))
                        (string-split rest #\space))))
             (checkers (map normalise-checker-name names)))
        (case [string->symbol directive]
          [(sift:disable-next-line) (cons 'disable-next-line checkers)]
          [(sift:disable) (cons 'disable checkers)]
          [(sift:enable) (cons 'enable checkers)]
          [else #f])))))

(define (suppression-directives ast)
  "Return the suppression directives in @var{ast}, in document order.

Each directive is a list of the kind, the checker names it names (empty for
all of them) and the line it sits on."
  (let ((directives '()))
    (define (walk node)
      (cond
        [(node-list? node) (for-each walk (node-children node))]
        [(node-prefix? node) (let ((child (node-child node)))
                               (when child (walk child)))]
        [(node-comment? node)
         (let ((directive (parse-directive (node-value node))))
           (when directive
             (set! directives
               (cons (list (car directive)
                       (cdr directive)
                       (source-location-line (node-start node))
                       (source-location-index (node-start node)))
                 directives))))]))
    (for-each walk ast)
    (reverse directives)))

(define (top-level-form-at ast index)
  "Return the top-level form of @var{ast} that contains character @var{index}."
  (find (lambda (node)
          (and (node-data? node)
            (<= (node-index node) index)
            (>= (node-end-index node) index)))
    ast))

(define (suppression-intervals ast)
  "Return the intervals a `sift:disable` covers, as @code{(start end . checkers)}.

A `disable` covers from where it is written to where the top-level form it
sits in ends, or to the end of the file when it sits between forms.  An
`enable` closes the matching interval at the comment that carries it."
  (let ((open '())
        (next-lines '()))
    (for-each
       (lambda (directive)
         (match directive
           ((kind checkers line index)
            (case kind
              [(disable-next-line)
               (set! next-lines
                 (cons (list (+ line 1) checkers) next-lines))]
              [(disable)
               (let* ((form (top-level-form-at ast index))
                      (end (if form (node-end-index form) (string-length ""))))
                 (set! open (cons (list checkers index end) open)))]
              [(enable)
               (set! open
                 (remove (lambda (entry)
                           (or (null? checkers)
                             (lset= eq? (first entry) checkers)))
                   open))]))))
       (suppression-directives ast))
    (values open next-lines)))

(define (checkers-match? directive-checkers checker)
  (or (null? directive-checkers)
    (memq checker directive-checkers)))

(define* (suppressed? warning #:key intervals next-lines)
  "Return @code{#t} when @var{warning} is silenced by a directive comment."
  (let* ((location (lint-warning-location warning))
         (checker (lint-warning-checker warning))
         (line (source-location-line location))
         (index (source-location-index location)))
    (or (any (lambda (entry)
               (and (checkers-match? (second entry) checker)
                 (>= index (second entry))
                 (<= index (third entry))))
          intervals)
      (any (lambda (entry)
             (and (= line (first entry))
               (checkers-match? (second entry) checker)))
        next-lines))))

;;; ===== Running checkers =====

(define (reader-warning-covered? warning)
  "Return @code{#t} for a reader diagnostic a checker already reports.

`unmatched-parens` reports the two delimiter problems from the AST, so the
reader's copy of the same finding is dropped and a run shows one line per
problem, whichever way it was selected."
  (let ((message (lint-warning-message warning)))
    (or (string-prefix? "unmatched " message)
      (string-prefix? "unterminated form" message))))

(define* (lint-source source #:key (dialect 'scheme) (file #f)
           (checkers #f) (exclude-checkers '()) (severity 'info)
           (options '()))
  "Lint @var{source} and return the warnings, sorted.

@var{checkers} selects the checkers to run; @var{exclude-checkers} removes
some; @var{severity} is the minimum severity to report.  @var{options} is
the alist a checker sees through @code{option} -- the line limit, the
indentation width, the LOOP keyword style."
  (call-with-values
    (lambda () (read-source #:source source #:dialect dialect #:file file))
    (lambda (ast parse-errors)
      (let* ((selected (checkers-applicable? dialect
                         #:include checkers
                         #:exclude exclude-checkers))
             (produced
                (append
                   (filter (lambda (warning) (not (reader-warning-covered? warning)))
                     parse-errors)
                   (append-map
                      (lambda (checker)
                        (with-check-options
                           options
                           (lambda ()
                             ((lint-checker-proc checker) ast source dialect))))
                      selected)))
             (with-file
                (map (lambda (warning)
                       (let ((location (lint-warning-location warning)))
                         (if (or (not file) (source-location-file location))
                           warning
                           (make-lint-warning
                              (lint-warning-checker warning)
                              (lint-warning-severity warning)
                              (lint-warning-message warning)
                              (make-source-location file
                                (source-location-line location)
                                (source-location-column location)
                                (source-location-offset location)
                                (source-location-index location))
                              (lint-warning-fix warning)
                              (lint-warning-range warning)))))
                  produced)))
        (call-with-values (lambda () (suppression-intervals ast))
          (lambda (intervals next-lines)
            (sort (filter (lambda (warning)
                            (and (severity-at-least? (lint-warning-severity warning)
                                   severity)
                              (not (suppressed? warning
                                     #:intervals intervals
                                     #:next-lines next-lines))))
                    with-file)
              lint-warning<?)))))))

(define (severity-at-least? severity minimum)
  "Return @code{#t} when @var{severity} is at or above @var{minimum}."
  (>= (severity-rank severity) (severity-rank minimum)))

(define* (lint-string source #:key (dialect 'scheme)
           (checkers #f) (exclude-checkers '())
           (severity 'info) (options '())
           #:allow-other-keys)
  "Lint @var{source} as text; a convenience over @code{lint-source}."
  (lint-source source #:dialect dialect #:checkers checkers
    #:exclude-checkers exclude-checkers #:severity severity
    #:options options))

(define* (lint-file path #:key (dialect #f) (checkers #f)
           (exclude-checkers '()) (severity 'info) (options '()))
  "Lint the file at @var{path} and return the warnings, sorted."
  (let* ((source (call-with-input-file path get-string-all))
         (dialect* (or dialect
                     (detect-dialect #:file path #:source source)
                     'scheme)))
    (lint-source source #:dialect dialect* #:file path
      #:checkers checkers #:exclude-checkers exclude-checkers
      #:severity severity #:options options)))

(define* (lint-files paths #:key (dialect #f) (checkers #f)
           (exclude-checkers '()) (severity 'info) (options '()))
  "Lint every file in @var{paths}, returning one list of warnings.

The warnings of the whole run are sorted together, so the report reads in
file order and a pipeline can compare runs line by line."
  (sort (append-map (lambda (path)
                      (lint-file path #:dialect dialect
                        #:checkers checkers
                        #:exclude-checkers exclude-checkers
                        #:severity severity #:options options))
          paths)
    lint-warning<?))

;;; ===== Fixes =====

(define (fixable-warnings warnings)
  "Return the warnings that carry both a fix and the range it replaces."
  (filter (lambda (warning)
            (and (lint-warning-fix warning) (lint-warning-range warning)))
    warnings))

(define* (apply-fixes source warnings)
  "Return @var{source} with every fix applied, or @var{source} unchanged.

Fixes are applied from the highest offset down, so that a fix does not move
the text a later one was measured against.  After applying them the source
is read again: a fix that introduces a parse error the file did not have
before is not worth the change, so all of them are discarded together and
the caller reports the original warnings."
  (let* ((fixes (sort (fixable-warnings warnings)
                  (lambda (a b)
                    (> (car (lint-warning-range a))
                      (car (lint-warning-range b))))))
         (fixed
            (fold (lambda (warning text)
                    (let* ((range (lint-warning-range warning))
                           (start (car range))
                           (end (cdr range))
                           (replacement ((lint-warning-fix warning)
                                         source start end)))
                      (string-append (substring text 0 start)
                        replacement
                        (substring text end))))
              source
              fixes)))
    (if (and (not (string=? fixed source))
          (fix-introduces-error? source fixed))
      source
      fixed)))

(define (fix-introduces-error? before after)
  "Return @code{#t} when @var{after} does not read as cleanly as @var{before}."
  (let ((before-errors (call-with-values
                         (lambda () (read-source #:source before))
                         (lambda (ast errors)
                           (length (filter (lambda (warning)
                                             (eq? (lint-warning-severity warning)
                                               'error))
                                     errors)))))
        (after-errors (call-with-values
                        (lambda () (read-source #:source after))
                        (lambda (ast errors)
                          (length (filter (lambda (warning)
                                            (eq? (lint-warning-severity warning)
                                              'error))
                                    errors))))))
    (> after-errors before-errors)))
