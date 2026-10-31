;; sift --- reporting
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
              json-escape
              sift-rules-page))

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
  (let* ((location (lint-warning-location warning))
         (checker (lint-warning-checker warning)))
    (string-append
       "{"
       "\"file\":" (json-escape (or (source-location-file location) "<stdin>")) ","
       "\"line\":" (number->string (source-location-line location)) ","
       "\"column\":" (number->string (source-location-column location)) ","
       "\"checker\":" (json-escape (symbol->string checker)) ","
       ;; the rule code is what an editor attaches to a diagnostic, so that a
       ;; key press can open the documentation of the rule that fired; a
       ;; finding that is not a rule -- a file that would be reformatted, one
       ;; that could not be read -- is named by its checker instead, because
       ;; an empty code is a code no editor can look up
       "\"code\":" (json-escape (or (rule-code checker)
                                  (symbol->string checker))) ","
       "\"severity\":" (json-escape (severity->string
                                       (lint-warning-severity warning))) ","
       "\"message\":" (json-escape (lint-warning-message warning))
       "}")))

;;; ===== SARIF =====
;;;
;;; SARIF 2.1.0 is what GitHub code scanning, DefectDojo and a dozen other
;;; tools read, so the document is written to the specification rather than to
;;; what one consumer happens to need.  A *run* names its tool, the rules the
;;; tool applied, the artifacts it looked at and the results it found; every
;;; result points at its rule and at its artifact by index, so that a document
;;; with a hundred findings describes each rule once.

(define sarif-schema-uri "https://json.schemastore.org/sarif-2.1.0.json")
(define sift-home-page "https://github.com/jjba23/sift")

(define (sift-rules-page)
  "Return the URL of the rule catalogue."
  (string-append sift-home-page "/blob/trunk/doc/rules.org"))

;;; --- building JSON -----------------------------------------------------

;;; Every object below is built from a list of members rather than by
;;; concatenating into one format string: the order of the keys is then the
;;; order they are written in, a member whose value is not known is #f, and
;;; `json-object' drops it.  That matters here because half of SARIF is
;;; optional: a region has a character span when the checker knew one, and a
;;; document with `"charOffset":#f' in it is not a document.

(define (json-array items)
  "Return @var{items} as a JSON array."
  (string-append "[" (string-join items ",") "]"))

(define (json-object . members)
  "Return a JSON object of @var{members}, in the order given.

A member that is @code{#f} is left out, which is how an optional field is
written."
  (string-append "{" (string-join (filter identity members) ",") "}"))

(define (json-field name value)
  "Return the JSON member @var{name} with @var{value}, or @code{#f} for none."
  (and value (string-append (json-escape name) ":" value)))

(define (json-text text)
  "Return @var{text} as a SARIF message object."
  (json-object (json-field "text" (json-escape text))))

(define (fingerprint . parts)
  "Return a stable short hash of @var{parts}, in hexadecimal.

The hash is written out here rather than taken from the runtime: a code
scanning service stores the fingerprint and compares it with the next run's,
so it may not change when this Guile changes.  FNV-1a is small enough to read
and stable by definition."
  (let ((text (string-join (map (lambda (part) (format #f "~a" part)) parts)
                "\n")))
    (let loop ((index 0) (hash #x811c9dc5))
      (if (>= index (string-length text))
        (format #f "~8,'0x" hash)
        (loop (+ index 1)
          (modulo (* (logxor hash
                       (char->integer (string-ref text index)))
                    #x01000193)
            #x100000000))))))

(define (distinct items)
  "Return @var{items} without duplicates, keeping the first of each."
  (let loop ((rest items) (seen '()) (out '()))
    (cond
      [(null? rest) (reverse out)]
      [(memv (car rest) seen) (loop (cdr rest) seen out)]
      [else (loop (cdr rest) (cons (car rest) seen) (cons (car rest) out))])))

;;; --- describing a rule ------------------------------------------------

;;; Two findings have a position and a message but no rule of the linter
;;; behind them: a file that could not be read, and a file that is not
;;; formatted.  They are results all the same, and a result whose rule is
;;; described nowhere is a result a code scanning service may discard, so
;;; they get a rule of their own.

(define pseudo-rules
  `((reader . "the file could not be read as Lisp")
    (format . "the file is not formatted the way sift would format it")))

(define (checker-dialect-names checker)
  "Return the dialects @var{checker} applies to, as names."
  (let ((applies (lint-checker-dialects checker)))
    (map symbol->string (if (eq? applies 'all) dialects applies))))

(define (dialects-sentence names)
  "Return @var{names} as an English list: scheme, elisp and cl."
  (cond
    [(null? names) "any dialect"]
    [(null? (cdr names)) (car names)]
    [else (string-append (string-join (drop-right names 1) ", ")
            " and " (car (take-right names 1)))]))

(define (rule-fields name)
  "Return what a SARIF document says about the rule @var{name}.

A rule of the linter answers with the fields of its checker record, so the
help a code scanning service shows is the description and the example the
test suite already asserts.  A pseudo rule -- `reader', `format' -- answers
from the table above, because there is no record to read."
  (let ((checker (find-checker name)))
    (if checker
      `((id . ,(or (lint-checker-code checker) (symbol->string name)))
        (name . ,(symbol->string (lint-checker-name checker)))
        (description . ,(lint-checker-description checker))
        (severity . ,(lint-checker-severity checker))
        (dialects . ,(checker-dialect-names checker))
        (example . ,(lint-checker-example checker)))
      `((id . ,(symbol->string name))
        (name . ,(symbol->string name))
        (description . ,(or (assq-ref pseudo-rules name) (format #f "~a" name)))
        (severity . warning)
        (dialects . ,(map symbol->string dialects))
        (example . #f)))))

(define (sentence text)
  "Return @var{text} with a full stop, unless it already ends with one."
  (if (or (string-null? text)
        (char=? (string-ref text (- (string-length text) 1)) #\.))
    text
    (string-append text ".")))

(define (indent-block text)
  "Return @var{text} with every line but the first indented by two spaces."
  (string-join (string-split text #\newline) "\n  "))

(define (rule-help-text rule)
  "Return the help text of @var{rule}: what it wants, and what it looks like.

The example is the rule's own -- the one its tests assert -- so that the help
shown beside a finding cannot drift from the rule."
  (let* ((code (assq-ref rule 'id))
         (example (assq-ref rule 'example))
         (input (and example (assq-ref example 'input)))
         (fix (and example (assq-ref example 'fix))))
    (string-append
       (assq-ref rule 'description) "\n\n"
       (if input
         (string-append "Reported, for example, for:\n\n  " (indent-block input)
           "\n\n"
           (if fix
             (string-append "which is written:\n\n  " (indent-block fix) "\n\n")
             ""))
         "")
       (format #f "Rule ~a is documented with the others at ~a."
         code (sift-rules-page)))))

(define (sarif-level severity)
  (case severity
    [(error) "error"]
    [(warning) "warning"]
    [else "note"]))

(define (sarif-rule name)
  "Return the SARIF description of the rule @var{name}."
  (let* ((rule (rule-fields name))
         (code (assq-ref rule 'id))
         (dialects (assq-ref rule 'dialects)))
    (json-object
       (json-field "id" (json-escape code))
       (json-field "name" (json-escape (assq-ref rule 'name)))
       (json-field "shortDescription" (json-text (assq-ref rule 'description)))
       (json-field "fullDescription"
         (json-text (format #f "~a Reported in ~a."
                      (sentence (assq-ref rule 'description))
                      (dialects-sentence dialects))))
       (json-field "help" (json-text (rule-help-text rule)))
       (json-field "helpUri"
         (if (assq-ref pseudo-rules name)
           (json-escape sift-home-page)
           (json-escape (format #f "~a#~a" (sift-rules-page) code))))
       (json-field "defaultConfiguration"
         (json-object
            (json-field "level" (json-escape (sarif-level (assq-ref rule 'severity))))))
       (json-field "properties"
         (json-object
            (json-field "tags" (json-array (map json-escape (cons "sift" dialects)))))))))

;;; --- describing the run -----------------------------------------------

(define (artifact-of warning)
  "Return the artifact @var{warning} is about, as it was named."
  (or (source-location-file (lint-warning-location warning)) "<stdin>"))

(define (artifact-uri path)
  "Return @var{path} as a URI a code scanning service can resolve.

A path relative to where sift ran is what such a service wants, because it
resolves it against the repository.  An absolute one is not, so it is spelled
as the `file:' URI it actually is."
  (if (and (string-prefix? "/" path) (not (string-prefix? "//" path)))
    (string-append "file://" path)
    path))

(define (sarif-artifact path)
  (json-object
     (json-field "location"
       (json-object (json-field "uri" (json-escape (artifact-uri path)))))
     (json-field "roles" (json-array (list (json-escape "analysisTarget"))))))

(define (parse-error? warning)
  "Return @code{#t} when @var{warning} says the file could not be read."
  (and (eq? (lint-warning-checker warning) 'reader)
    (eq? (lint-warning-severity warning) 'error)))

(define (sarif-invocation warnings)
  "Return the invocation of the run that found @var{warnings}.

`executionSuccessful' is false when a file could not be parsed: the analysis
of that file did not finish, and a document that claimed otherwise would be
saying something sift does not know."
  (json-object
     (json-field "executionSuccessful"
       (if (any parse-error? warnings) "false" "true"))))

;;; --- results ----------------------------------------------------------

(define (sarif-region warning)
  "Return the region @var{warning} points at.

The line and the column are always there; the character span is there when
the checker said which characters it meant, which is what lets a consumer
highlight the whole finding rather than one character of it."
  (let* ((location (lint-warning-location warning))
         (range (lint-warning-range warning)))
    (json-object
       (json-field "startLine" (number->string (source-location-line location)))
       (json-field "startColumn" (number->string (source-location-column location)))
       (and range (json-field "charOffset" (number->string (car range))))
       (and range
         (json-field "charLength"
           (number->string (max 1 (- (cdr range) (car range)))))))))

(define (warning->sarif warning artifacts rules)
  "Return one warning as a SARIF result, pointing at its rule and its artifact."
  (let* ((checker (lint-warning-checker warning))
         (rule (assq-ref (rule-fields checker) 'id))
         (artifact (artifact-of warning))
         (rule-index (list-index (lambda (name) (eq? name checker)) rules))
         (artifact-index
            (list-index (lambda (path) (string=? path artifact)) artifacts)))
    (json-object
       (json-field "ruleId" (json-escape rule))
       (and rule-index (json-field "ruleIndex" (number->string rule-index)))
       (json-field "level"
         (json-escape (sarif-level (lint-warning-severity warning))))
       (json-field "message"
         (json-text (format #f "~a [~a]" (lint-warning-message warning) checker)))
       (json-field "locations"
         (json-array
            (list
               (json-object
                  (json-field "physicalLocation"
                    (json-object
                       (json-field "artifactLocation"
                         (json-object
                            (json-field "uri" (json-escape (artifact-uri artifact)))
                            (and artifact-index
                              (json-field "index" (number->string artifact-index)))))
                       (json-field "region" (sarif-region warning))))))))
       ;; The fingerprint is what a code scanning service offers the finding by:
       ;; the rule, the file and the message, and deliberately not the line, so
       ;; that adding a line above a finding does not open a new alert for it.
       (json-field "partialFingerprints"
         (json-object
            (json-field "siftFingerprint/v1"
              (json-escape (fingerprint rule artifact
                             (lint-warning-message warning)))))))))

(define* (warnings->sarif warnings)
  "Return @var{warnings} as a SARIF 2.1.0 document.

One run, whatever the number of files: the rules that fired, the artifacts
those findings are in, and the findings themselves.  A run that found nothing
is still a valid document, and one that could not read a file says so."
  (let* ((rules (distinct (map lint-warning-checker warnings)))
         (artifacts (distinct (map artifact-of warnings)))
         (results (map (lambda (warning)
                         (warning->sarif warning artifacts rules))
                    warnings)))
    (json-object
       (json-field "$schema" (json-escape sarif-schema-uri))
       (json-field "version" (json-escape "2.1.0"))
       (json-field "runs"
         (json-array
            (list
               (json-object
                  (json-field "tool"
                    (json-object
                       (json-field "driver"
                         (json-object
                            (json-field "name" (json-escape "sift"))
                            (json-field "fullName"
                              (json-escape "sift, a dialect-aware formatter and linter"))
                            (json-field "version" (json-escape sift-version))
                            (json-field "informationUri" (json-escape sift-home-page))
                            (json-field "rules"
                              (json-array (map sarif-rule rules)))))))
                  (json-field "invocations"
                    (json-array (list (sarif-invocation warnings))))
                  (json-field "artifacts"
                    (json-array (map sarif-artifact artifacts)))
                  (json-field "columnKind" (json-escape "unicodeCodePoints"))
                  (json-field "results" (json-array results)))))))))

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
