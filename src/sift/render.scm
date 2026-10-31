;; sift --- reporting
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Everything that turns a warning into something a human or a program
;; reads: the one-line report in the style of Guix lint, the JSON array, the
;; table `list-checkers' prints, the page `explain' prints, and the unified
;; diff `check' prints for a file that would change.
;;
;; Colour is used only when the output is a terminal and NO_COLOR is unset,
;; so a pipeline never has to strip escape sequences it did not ask for.

(define-module (sift render)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 match)
  #:use-module (sift records)
  ;; The registry, so that a SARIF document can describe the rules it names.
  ;; `(sift lint)' does not import this module, so the dependency points one
  ;; way: the linter knows nothing about how it is printed.
  #:use-module (sift lint)
  #:export (color-enabled?
              paint
              render-warning
              render-warnings
              render-summary
              warnings->json
              warning->json
              list-checkers-table
              explain-checker
              unified-diff
              json-escape))

;;; ===== Colour =====

(define ansi-codes
  '((error . "31") (warning . "33") (info . "36")
    (ok . "32") (bold . "1") (reset . "0")))

(define* (color-enabled? #:key (no-color #f) (port (current-output-port)))
  "Return @code{#t} when colour should be written to @var{port}.

Three things can say no, and any one of them is enough: `--no-color', the
NO_COLOR environment variable, and an output that is not a terminal."
  (and (not no-color)
    (not (getenv "NO_COLOR"))
    (isatty? port)))

(define* (paint text code #:key enabled)
  "Wrap @var{text} in the ANSI @var{code} when @var{enabled}."
  (if enabled
    (string-append "\x1b[" (cdr (assq code ansi-codes)) "m" text "\x1b[0m")
    text))

;;; ===== The human report =====

(define* (render-warning warning #:key (color? #f) (port #f))
  "Render one warning as the line `file:line:column: severity: [checker] msg'.

The order is the one an editor's compile-mode parses: position first, then
severity, then the bracketed checker, then a message that is a lowercase
sentence with no trailing period."
  (let* ((location (lint-warning-location warning))
         (path (or (source-location-file location) "<stdin>"))
         (sev (lint-warning-severity warning))
         (line (format #f "~a:~a:~a: ~a: [~a] ~a"
                 (paint (format #f "~a" path) 'bold #:enabled color?)
                 (source-location-line location)
                 (source-location-column location)
                 (paint (severity->string sev) sev #:enabled color?)
                 (lint-warning-checker warning)
                 (lint-warning-message warning))))
    (if port (display line port) line)))

(define* (render-warnings warnings #:key (format 'human) (color? #f)
           (port (current-output-port)) (quiet #f))
  "Write @var{warnings} to @var{port} in @var{format}.

@code{json} writes one array for the whole run, which is what a streaming
consumer wants; @code{null} writes nothing at all and exists so that a
script can ask for the exit status alone."
  (case format
    [(json) (display (warnings->json warnings) port) (newline port)]
    [(github)
     (for-each (lambda (warning)
                 (display (warning->github warning) port)
                 (newline port))
       warnings)]
    [(sarif) (display (warnings->sarif warnings) port) (newline port)]
    [(null) *unspecified*]
    [else
       (for-each (lambda (warning)
                   (display (render-warning warning #:color? color?) port)
                   (newline port))
         warnings)]))

(define* (render-summary files warnings errors infos
           #:key (color? #f) (port (current-output-port)))
  "Write the `N files checked, N warnings, N errors` line."
  (let ((line (format #f "~a ~a, ~a, ~a"
                files
                (if (= files 1) "file checked" "files checked")
                (format #f "~a ~a" warnings
                  (if (= warnings 1) "warning" "warnings"))
                (format #f "~a ~a" errors
                  (if (= errors 1) "error" "errors")))))
    (display (paint line (if (> errors 0) 'error
                           (if (> warnings 0) 'warning 'ok))
               #:enabled color?)
      port)
    (newline port)))

;;; ===== JSON =====

(define (json-escape string)
  "Return @var{string} as a JSON string body, quotes and all."
  (let ((out (open-output-string)))
    (display "\"" out)
    (string-for-each
       (lambda (c)
         (case c
           [(#\") (display "\\\"" out)]
           [(#\\) (display "\\\\" out)]
           [(#\newline) (display "\\n" out)]
           [(#\return) (display "\\r" out)]
           [(#\tab) (display "\\t" out)]
           [else
              (if (char<? c #\space)
                (format out "\\u~4,'0x" (char->integer c))
                (display c out))]))
       string)
    (display "\"" out)
    (get-output-string out)))

(define* (warning->json warning)
  "Return one warning as a JSON object, its keys in a fixed order.

The order matters: a diff of two runs of sift should only show what
changed, and a hash table would not promise that."
  (let ((location (lint-warning-location warning)))
    (string-append
       "{"
       "\"file\":" (json-escape (or (source-location-file location) "<stdin>")) ","
       "\"line\":" (number->string (source-location-line location)) ","
       "\"column\":" (number->string (source-location-column location)) ","
       "\"checker\":" (json-escape (symbol->string
                                      (lint-warning-checker warning))) ","
       ;; the rule code is what an editor attaches to a diagnostic, so that a
       ;; key press can open the documentation of the rule that fired
       "\"code\":" (json-escape (or (rule-code (lint-warning-checker warning))
                                  "")) ","
       "\"severity\":" (json-escape (severity->string
                                       (lint-warning-severity warning))) ","
       "\"message\":" (json-escape (lint-warning-message warning))
       "}")))

;;; ===== SARIF =====
;;;
;;; SARIF 2.1.0 is what GitHub code scanning, DefectDojo and a dozen other
;;; tools read.  A result names the rule that fired, its level, the message
;;; and the place; the rules it refers to are described once, in the order
;;; they first appear, which is what `ruleIndex' points at.

(define (sarif-level severity)
  (case severity
    [(error) "error"]
    [(warning) "warning"]
    [else "note"]))

(define* (rules-of warnings)
  "Return the checkers the run mentioned, in the order they first appear."
  (let ((seen '()))
    (reverse
       (filter-map
          (lambda (warning)
            (let ((name (lint-warning-checker warning)))
              (unless (memq name seen)
                (set! seen (cons name seen))
                (find-checker name))))
          warnings))))

(define (sarif-rule checker)
  (string-append
     "{\"id\":" (json-escape (or (lint-checker-code checker) "")) ","
     "\"name\":" (json-escape (symbol->string (lint-checker-name checker))) ","
     "\"shortDescription\":{\"text\":"
     (json-escape (lint-checker-description checker)) "},"
     "\"defaultConfiguration\":{\"level\":"
     (json-escape (sarif-level (lint-checker-severity checker))) "},"
     "\"properties\":{\"tags\":["
     (json-escape (dialects->string checker)) "]}}"))

(define* (warning->sarif warning #:optional (rules '()))
  (let* ((location (lint-warning-location warning))
         (checker (lint-warning-checker warning))
         (index (list-index (lambda (rule)
                              (eq? (lint-checker-name rule) checker))
                  rules)))
    (string-append
       "{"
       "\"ruleId\":" (json-escape (or (rule-code checker) "")) ","
       (if index
         (string-append "\"ruleIndex\":" (number->string index) ",")
         "")
       "\"level\":" (json-escape (sarif-level (lint-warning-severity warning)))
       ","
       "\"message\":{\"text\":"
       (json-escape (format #f "~a [~a]"
                      (lint-warning-message warning) checker))
       "},"
       "\"locations\":[{\"physicalLocation\":{"
       "\"artifactLocation\":{\"uri\":"
       (json-escape (or (source-location-file location) "<stdin>")) "},"
       "\"region\":{\"startLine\":" (number->string (source-location-line location))
       ",\"startColumn\":" (number->string (source-location-column location)) "}}}]"
       "}")))

(define* (warnings->sarif warnings)
  "Return @var{warnings} as a SARIF 2.1.0 document."
  (let* ((rules (rules-of warnings))
         (results (map (lambda (warning) (warning->sarif warning rules))
                    warnings)))
    (string-append
       "{\"$schema\":\"https://json.schemastore.org/sarif-2.1.0.json\","
       "\"version\":\"2.1.0\","
       "\"runs\":[{\"tool\":{\"driver\":{"
       "\"name\":\"sift\","
       "\"version\":" (json-escape sift-version) ","
       "\"informationUri\":\"https://codeberg.org/jjba23/sift\","
       "\"rules\":[" (string-join (map sarif-rule rules) ",") "]"
       "}},"
       "\"results\":[" (string-join results ",") "]"
       "}]}")))

(define* (warning->github warning)
  "Return one warning as a GitHub Actions workflow command.

An annotation puts the finding on the line of the diff in a pull request,
which is where a reviewer is already looking.  The rule code goes in the
title so that the same handle as everywhere else is on screen."
  (let* ((location (lint-warning-location warning))
         (severity (lint-warning-severity warning))
         (kind (case severity
                 [(error) "error"]
                 [(warning) "warning"]
                 [else "notice"]))
         (title (format #f "~a [~a]"
                  (or (rule-code (lint-warning-checker warning)) "")
                  (lint-warning-checker warning))))
    (format #f "::~a file=~a,line=~a,col=~a,title=~a::~a"
      kind
      (or (source-location-file location) "<stdin>")
      (source-location-line location)
      (source-location-column location)
      (github-escape title)
      (github-escape (lint-warning-message warning)))))

(define (github-escape text)
  "Escape the characters a workflow command treats as syntax."
  (string-replace-substring
     (string-replace-substring
        (string-replace-substring text "%" "%25")
        "\r" "%0D")
     "\n" "%0A"))

(define (warnings->json warnings)
  "Return @var{warnings} as a JSON array; an empty run is `[]', not blank."
  (string-append "["
    (string-join (map warning->json warnings) ",")
    "]"))

;;; ===== list-checkers and explain =====

(define (dialects->string checker)
  (let ((dialects (lint-checker-dialects checker)))
    (if (eq? dialects 'all)
      "all"
      (string-join (map symbol->string dialects) ","))))

(define* (list-checkers-table checkers #:key (port (current-output-port)))
  "Write the table `list-checkers' prints.

The columns are as wide as their contents need, with the widths the manual
shows as a minimum: a checker whose name is cut in half is a checker nobody
can select, because the name printed is the name `--checker' takes."
  (let* ((name-width
            (max 24
              (+ 2
                (apply max
                  0
                  (map (lambda (checker)
                         (string-length (symbol->string
                                           (lint-checker-name checker))))
                    checkers)))))
         (dialect-width
            (max 15
              (+ 2
                (apply max
                  0
                  (map (lambda (checker)
                         (string-length (dialects->string checker)))
                    checkers)))))
         (severity-width 10)
         (code-width 9))
    (format port "~a ~a ~a ~a ~a~%"
      (string-pad-right "NAME" name-width)
      (string-pad-right "CODE" code-width)
      (string-pad-right "DIALECTS" dialect-width)
      (string-pad-right "SEVERITY" severity-width)
      "DESCRIPTION")
    (for-each
       (lambda (checker)
         (format port "~a ~a ~a ~a ~a~%"
           (string-pad-right (symbol->string (lint-checker-name checker))
             name-width)
           (string-pad-right (or (lint-checker-code checker) "") code-width)
           (string-pad-right (dialects->string checker) dialect-width)
           (string-pad-right (symbol->string (lint-checker-severity checker))
             severity-width)
           (lint-checker-description checker)))
       checkers)))

(define* (explain-checker checker #:key (port (current-output-port)))
  "Write everything sift knows about @var{checker}."
  (let ((example (lint-checker-example checker)))
    (format port "CODE~%~a~%~%" (or (lint-checker-code checker) ""))
    (format port "NAME~%~a~%~%" (lint-checker-name checker))
    (format port "DESCRIPTION~%~a~%~%" (lint-checker-description checker))
    (format port "DIALECTS~%~a~%~%"
      (let ((dialects (lint-checker-dialects checker)))
        (if (eq? dialects 'all)
          "all dialects: scheme, elisp, cl"
          (string-join (map symbol->string dialects) ", "))))
    (format port "SEVERITY~%~a~%~%" (lint-checker-severity checker))
    (when (and example (assq 'input example))
      (format port "FAILING EXAMPLE~%~a~%~%" (assq-ref example 'input)))
    (when (and example (assq 'fix example))
      (format port "EXPECTED FIX~%~a~%" (assq-ref example 'fix)))))

;;; ===== Unified diff =====

(define (source-line-list text)
  (let ((lines (string-split text #\newline)))
    (if (and (pair? lines) (string-null? (car (reverse lines))))
      (drop-right lines 1)
      lines)))

(define (common-prefix a b)
  (let loop ((a a) (b b) (n 0))
    (if (and (pair? a) (pair? b) (string=? (car a) (car b)))
      (loop (cdr a) (cdr b) (+ n 1))
      n)))

(define (common-suffix a b)
  (let loop ((a (reverse a)) (b (reverse b)) (n 0))
    (if (and (pair? a) (pair? b) (string=? (car a) (car b)))
      (loop (cdr a) (cdr b) (+ n 1))
      n)))

(define* (unified-diff old-text new-text #:key (path "file") (context 3)
           (color? #f) (labels #f))
  "Return a unified diff between @var{old-text} and @var{new-text}.

The diff is not the shortest one possible -- it trims the common prefix and
suffix and reports the rest as one hunk -- because sift's job is to say
\"this file would change\", which is what the reader of a `check' run needs,
not to minimise a patch."
  (let* ((old (source-line-list old-text))
         (new (source-line-list new-text))
         (prefix (common-prefix old new))
         (suffix (common-suffix (drop old prefix) (drop new prefix)))
         (old-changed (drop-right (drop old prefix) suffix))
         (new-changed (drop-right (drop new prefix) suffix)))
    (if (and (null? old-changed) (null? new-changed))
      ""
      (let* ((window-start (max 0 (- prefix context)))
             (old-end (min (length old)
                        (+ (- (length old) suffix) context)))
             (new-end (min (length new)
                        (+ (- (length new) suffix) context)))
             (old-count (- old-end window-start))
             (new-count (- new-end window-start))
             (out (open-output-string))
             ;; A single file is labelled by its own name, which is what
             ;; `patch' and `git apply' both accept; when two labels are
             ;; given they are used as they are.
             (old-label (if labels (car labels) path))
             (new-label (if labels (cdr labels) path)))
        (format out "--- ~a~%" old-label)
        (format out "+++ ~a~%" new-label)
        (format out "@@ -~a,~a +~a,~a @@~%"
          (+ window-start 1) old-count (+ window-start 1) new-count)
        (for-each (lambda (line)
                    (display (string-append " " line) out)
                    (newline out))
          (take (drop old window-start) (- prefix window-start)))
        (for-each (lambda (line)
                    (display (paint (string-append "-" line) 'error
                               #:enabled color?) out)
                    (newline out))
          old-changed)
        (for-each (lambda (line)
                    (display (paint (string-append "+" line) 'ok
                               #:enabled color?) out)
                    (newline out))
          new-changed)
        (let* ((tail-start (- (length old) suffix))
               (tail (take (drop old tail-start)
                       (max 0 (- old-end tail-start)))))
          (for-each (lambda (line)
                      (display (string-append " " line) out)
                      (newline out))
            tail))
        (get-output-string out)))))
