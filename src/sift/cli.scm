;; sift --- command line interface
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The command line is parsed with SRFI-37 `args-fold', which is what makes
;; abbreviated long options and a `--' that stops parsing work the way a
;; Unix user expects, without the bookkeeping `getopt-long' needs for
;; optional arguments.
;;
;; Two promises are kept here and nowhere else:
;;
;;   * nothing is written to disk unless `--write' asks for it, and
;;   * the exit status is 0 for clean, 1 for findings or formatting
;;     differences, and 2 for a usage error, an unreadable file or a file
;;     that cannot be parsed at all.
;;
;; The status is the same whether the run is a person typing or a Makefile
;; calling, which is what makes sift usable in CI and in an editor's
;; format-on-save hook without special cases.

(define-module (sift cli)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift format)
  #:use-module (sift lint)
  #:use-module (sift render)
  #:use-module (sift config)
  ;; `sift-version' lives in (sift records); re-exporting it is how the
  ;; executable and the library agree about which version this is
  #:re-export (sift-version)
  #:export (sift-main
              usage-text
              parse-options
              build-overrides))

(define usage-text
  "Usage:
  sift format [OPTIONS] [FILE...]
  sift lint [OPTIONS] [FILE...]
  sift check [OPTIONS] [FILE...]
  sift fix [OPTIONS] [FILE...]        # lint --fix
  sift ci [OPTIONS] [FILE-OR-DIR...]  # format check + lint; exits 1 if either fails
  sift install-hooks [--force]        # a pre-commit hook that runs `sift ci`
  sift doctor                         # what ran, what it read, what it decided
  sift list-checkers [--dialect DIALECT]
  sift explain CHECKER-NAME
  sift config show|explain|trust ...
  sift version

Options common to the commands: --dialect, --brackets, --line-length,
--style, --severity, --format, --color/--no-color, --verbose, --config.

Run `sift COMMAND --help' for the options of a command.
")

;;; ===== Usage errors =====

(define (usage-error template . args)
  (throw 'sift-usage-error (apply format #f template args)))

;;; ===== Option parsing =====

(define option-specification
  (list
     (option '(#\w "write") #f #f
       (lambda (opt name arg state) (acons 'write #t state)))
     (option '(#\c "check") #f #f
       (lambda (opt name arg state) (acons 'check #t state)))
     (option '("stdin") #f #f
       (lambda (opt name arg state) (acons 'stdin #t state)))
     (option '("stdout") #f #f
       (lambda (opt name arg state) (acons 'stdout #t state)))
     (option '("fix") #f #f
       (lambda (opt name arg state) (acons 'fix #t state)))
     (option '("diff") #f #f
       (lambda (opt name arg state) (acons 'diff #t state)))
     (option '("label") #t #f
       (lambda (opt name arg state) (acons 'label arg state)))
     (option '("brackets") #t #f
       (lambda (opt name arg state) (acons 'brackets arg state)))
     (option '(#\V "version") #f #f
       (lambda (opt name arg state) (acons 'version #t state)))
     (option '("comment-align") #f #f
       (lambda (opt name arg state) (acons 'comment-align #t state)))
     (option '("no-color") #f #f
       (lambda (opt name arg state) (acons 'color 'never state)))
     (option '("force") #f #f
       (lambda (opt name arg state) (acons 'force #t state)))
     (option '("trust-config") #f #f
       (lambda (opt name arg state) (acons 'trust-config #t state)))
     (option '(#\q "quiet") #f #f
       (lambda (opt name arg state) (acons 'quiet #t state)))
     (option '(#\v "verbose") #f #f
       (lambda (opt name arg state) (acons 'verbose #t state)))
     (option '("help") #f #f
       (lambda (opt name arg state) (acons 'help #t state)))
     (option '("dialect") #t #f
       (lambda (opt name arg state) (acons 'dialect arg state)))
     (option '("severity") #t #f
       (lambda (opt name arg state) (acons 'severity arg state)))
     (option '("format") #t #f
       (lambda (opt name arg state) (acons 'format arg state)))
     (option '("style") #t #f
       (lambda (opt name arg state) (acons 'style arg state)))
     (option '("line-length") #t #f
       (lambda (opt name arg state) (acons 'line-length arg state)))
     (option '("indent") #t #f
       (lambda (opt name arg state) (acons 'indent arg state)))
     (option '("config") #t #f
       (lambda (opt name arg state) (acons 'config arg state)))
     (option '("checker") #t #f
       (lambda (opt name arg state)
         (acons 'checkers (cons arg (or (assq-ref state 'checkers) '())) state)))
     (option '("exclude-checker") #t #f
       (lambda (opt name arg state)
         (acons 'exclude-checkers
           (cons arg (or (assq-ref state 'exclude-checkers) '()))
           state)))))

(define long-option-names
  '("write" "check" "stdin" "stdout" "fix" "comment-align" "no-color" "force"
    "trust-config" "quiet" "verbose" "help" "dialect" "severity" "format"
    "style" "line-length" "indent" "config" "checker" "exclude-checker"))

(define long-options-with-values
  '("dialect" "severity" "format" "style" "line-length" "indent"
    "config" "checker" "exclude-checker"))

(define (resolve-long-name name)
  "Return the full long option @var{name} abbreviates, or @code{#f}.

Abbreviation is resolved here because the SRFI-37 implementation Guile
ships matches long names exactly; doing it in one place keeps `--dial',
`--line-len' and the full spellings interchangeable."
  (cond
    [(member name long-option-names) name]
    [else
       (let ((matches (filter (lambda (candidate)
                                (string-prefix? name candidate))
                        long-option-names)))
         (and (= 1 (length matches)) (car matches)))]))

(define (normalise-long-options args)
  "Rewrite `--name value' into `--name=value' and expand abbreviations.

The SRFI-37 reference implementation reads a long option's argument only
when it follows an `=', and matches long names exactly.  Normalising first
keeps the natural spelling and the abbreviation working while leaving
`args-fold' in charge of `--', the short flags and the fold itself -- and
it is the only place that knows which options take a value, so that list
cannot drift from the specification above it."
  (let loop ((rest args) (out '()))
    (cond
      [(null? rest) (reverse out)]
      [(string=? (car rest) "--") (append (reverse out) rest)]
      [(not (string-prefix? "--" (car rest)))
       (loop (cdr rest) (cons (car rest) out))]
      [else
         (let* ((arg (car rest))
                (equal-at (string-index arg #\=))
                (name (substring arg 2 (or equal-at (string-length arg))))
                (full (resolve-long-name name)))
           (cond
             [(not full) (loop (cdr rest) (cons arg out))]
             [(and (not equal-at)
                (member full long-options-with-values)
                (pair? (cdr rest)))
              (loop (cddr rest) (cons (string-append "--" full "=" (cadr rest)) out))]
             [(and (not equal-at) (not (member full long-options-with-values)))
              (loop (cdr rest) (cons (string-append "--" full) out))]
             [else
                (loop (cdr rest)
                  (cons (string-append "--" full
                          (if equal-at (substring arg equal-at) ""))
                    out))]))])))

(define* (parse-options args)
  "Parse @var{args} and return @code{(state . files)}.

@var{state} is an alist of what the command line said; @var{files} are the
operands, in the order they were written.  A `--' stops option parsing, so a
file whose name begins with a dash can still be named."
  (args-fold (normalise-long-options args)
    option-specification
    (lambda (opt name arg state)
      (usage-error "unrecognized option `~a'" name)
      state)
    (lambda (operand state) (acons 'operand operand state))
    '()))

;;; ===== Turning the command line into configuration =====

(define (checker-name-list value what)
  (map (lambda (part)
         (let ((trimmed (string-trim-both part)))
           (when (string-null? trimmed)
             (usage-error "~a needs a checker name" what))
           (string->symbol trimmed)))
    (append-map (lambda (piece) (string-split piece #\,))
      (string-split value #\space))))

(define* (build-overrides state)
  "Return the alist of settings the command line overrides.

Invalid values are usage errors, not silent fallbacks: a `--line-length' of
`abc' means the run is wrong, and saying so is the only useful answer."
  (define (number-option key what)
    (let ((value (assq-ref state key)))
      (and value
        (let ((n (string->number value)))
          (if (and n (integer? n) (> n 0))
            n
            (usage-error "~a must be a positive integer, got `~a'"
              what value))))))
  (append
     (filter identity
       (list
          (let ((value (assq-ref state 'dialect)))
            (and value
              (let ((dialect (string->dialect value)))
                (unless dialect
                  (usage-error "unknown dialect `~a'; supported dialects are scheme, elisp and cl"
                    value))
                (cons 'dialect dialect))))
          (let ((value (assq-ref state 'style)))
            (and value
              (let ((style (string->style value)))
                (unless style
                  (usage-error "unknown style `~a'; supported styles are community and dangling"
                    value))
                (cons 'style style))))
          (let ((value (assq-ref state 'severity)))
            (and value
              (let ((severity (string->severity value)))
                (unless severity
                  (usage-error "unknown severity `~a'; sift knows error, warning and info"
                    value))
                (cons 'severity severity))))
          (let ((value (assq-ref state 'format)))
            (and value
              (let ((output (string->symbol value)))
                (unless (memq output '(human json null github sarif))
                  (usage-error "unknown format `~a'; sift knows human, json, null, github and sarif"
                    value))
                (cons 'format output))))
          (let ((value (assq-ref state 'brackets)))
            (and value
              (let ((brackets (string->brackets value)))
                (unless brackets
                  (usage-error "unknown bracket style `~a'; sift knows square, parens and preserve"
                    value))
                (cons 'brackets brackets))))
          (let ((value (number-option 'line-length "--line-length")))
            (and value (cons 'line-length value)))
          (let ((value (number-option 'indent "--indent")))
            (and value (cons 'indent value)))
          (and (assq 'comment-align state) (cons 'comment-align #t))
          (and (assq 'quiet state) (cons 'quiet #t))
          (and (assq 'verbose state) (cons 'verbose #t))
          (and (assq 'force state) (cons 'force #t))
          (let ((value (assq-ref state 'color)))
            (and value (cons 'color value)))
          (let ((value (assq-ref state 'checkers)))
            (and value
              (cons 'checkers
                (delete-duplicates
                   (append-map (lambda (item)
                                 (checker-name-list item "--checker"))
                     (reverse value))))))
          (let ((value (assq-ref state 'exclude-checkers)))
            (and value
              (cons 'exclude-checkers
                (delete-duplicates
                   (append-map (lambda (item)
                                 (checker-name-list item "--exclude-checker"))
                     (reverse value))))))))
     '()))

(define lisp-file-extensions
  '(".scm" ".ss" ".sld" ".sls" ".el" ".lisp" ".cl" ".asd" ".lsp" ".system"))

(define (lisp-file? path)
  (let ((dot (string-rindex path #\.)))
    (and dot (member (substring path dot) lisp-file-extensions) #t)))

(define (directory-files* directory)
  "Return the Lisp files under @var{directory}, sorted.

Hidden directories and the usual build droppings are skipped, so
`sift ci .` in a project does what the person typing it meant."
  (append-map
     (lambda (name)
       (cond
         [(member name '("." ".." ".git" ".hg" ".svn" "vendor" "node_modules"
                         ".cache" ".tox" "dist" "build"))
          '()]
         [else
            (let ((path (string-append (string-trim-right directory #\/) "/" name)))
              (cond
                [(eq? (stat:type (stat path)) 'directory) (directory-files* path)]
                [(lisp-file? name) (list path)]
                [else '()]))]))
     (sort (scandir directory (lambda (name)
                                (not (member name '("." ".." ".git"))))) string<?)))

(define (expand-operands files)
  "Turn directory operands into the Lisp files they hold, in sorted order.

A linter is pointed at a project, not at a list: `sift ci .` and
`sift format --write src/` are what people type, so a directory is read as
\"every Lisp file under here\"."
  (append-map
     (lambda (path)
       (cond
         [(not (file-exists? path)) (list path)]
         [(eq? (stat:type (stat path)) 'directory) (directory-files* path)]
         [else (list path)]))
     files))

(define (operands state)
  (reverse (filter-map (lambda (entry)
                         (and (eq? (car entry) 'operand) (cdr entry)))
             state)))

;;; ===== Shared helpers =====

(define* (emit-verbose config fmt . args)
  (when (config-ref config 'verbose)
    (apply format (current-error-port) (string-append "sift: " fmt "~%") args)))

(define* (read-source-of path #:key (label #f))
  "Read @var{path}, or standard input when it is @code{#f}."
  (if path
    (call-with-input-file path get-string-all)
    (get-string-all (current-input-port))))

(define (unreadable-file path condition)
  (format (current-error-port) "sift: cannot read ~a: ~a~%"
    path (if (pair? condition) (cadr condition) condition))
  2)

(define* (safe-read-file path)
  "Read @var{path}; return @code{(values text #f)} or @code{(values #f status)}."
  (catch #t
    (lambda () (values (call-with-input-file path get-string-all) #f))
    (lambda (key . args) (values #f (unreadable-file path args)))))

(define* (dialect-for config path source #:key (label #f))
  "Return the dialect to read @var{source} as, or a usage error.

The order is the documented one: `--dialect', then the extension, then the
shebang, then a marker in the file.  When none of them answers, sift asks
rather than guesses, and says which dialects it knows."
  (or (config-ref config 'dialect)
    (and path (detect-dialect #:file path #:source source))
    (and source (detect-dialect #:source source))
    (usage-error "cannot tell the dialect of ~a; pass --dialect scheme|elisp|cl"
      (or label path "the input"))))

(define (parse-error-count warnings)
  (length (filter (lambda (warning)
                    (and (eq? (lint-warning-checker warning) 'reader)
                      (eq? (lint-warning-severity warning) 'error)))
            warnings)))

(define (print-parse-errors warnings)
  (for-each (lambda (warning)
              (format (current-error-port) "~a~%"
                (lint-warning->string warning)))
    (filter (lambda (warning)
              (and (eq? (lint-warning-checker warning) 'reader)
                (eq? (lint-warning-severity warning) 'error)))
      warnings)))

(define (deny-on-parse-errors config path source dialect)
  "Return the status 2 when @var{source} cannot be parsed and --force is off.

Formatting a file with a broken form would mean guessing where the missing
delimiter goes, and sift does not guess."
  (if (config-ref config 'force)
    #f
    (call-with-values (lambda () (read-source #:source source #:dialect dialect
                                   #:file path))
      (lambda (ast errors)
        (let ((errors (filter (lambda (warning)
                                (eq? (lint-warning-severity warning) 'error))
                        errors)))
          (if (null? errors)
            #f
            (begin
              (print-parse-errors errors)
              (format (current-error-port)
                "sift: ~a has parse errors; nothing was written (see --force)~%"
                (or path "<stdin>"))
              2)))))))

(define (make-directories path)
  "Create @var{path} and every missing parent, like `mkdir -p'."
  (unless (or (file-exists? path) (string=? path "/") (string-null? path))
    (make-directories (dirname path))
    (catch 'system-error
      (lambda () (mkdir path))
      (lambda (key . args)
        (unless (file-exists? path) (apply throw key args))))))

(define (write-string-to path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

;;; ===== format =====

(define* (format-one config path source #:key (write? #f) (check? #f)
           (label #f) (diff? #f))
  "Format one source, returning the exit status for it.

@var{path} is @code{#f} for standard input.  Nothing is written unless
@var{write?} asks for it; without it the formatted text goes to stdout, so
`format' is a filter."
  (let* ((dialect (dialect-for config path source #:label label))
         (style (config-ref config 'style))
         (indent (config-ref config 'indent))
         (align (config-ref config 'comment-align))
         (brackets (config-ref config 'brackets))
         (rules (config-ref config 'indent-rules)))
    (emit-verbose config "formatting ~a (~a, ~a)"
      (or path "<stdin>") dialect style)
    (let ((denied (deny-on-parse-errors config path source dialect)))
      (if denied
        denied
        (call-with-values
          (lambda ()
            (format-source source #:dialect dialect #:file path
              #:style style #:indent indent
              #:comment-align align #:brackets brackets
              #:indent-rules rules))
          (lambda (formatted errors)
            (cond
              [check?
                 (if (string=? formatted source)
                   (begin (emit-verbose config "~a is already formatted"
                            (or path "<stdin>"))
                     0)
                   (begin
                     (case [config-ref config 'format]
                       [(json)
                        (format #t "[{\"file\":~a,\"changed\":true}]~%"
                          (json-escape (or path "<stdin>")))]
                       [(null) *unspecified*]
                       [(github)
                        (let ((name (or path label "<stdin>")))
                          (format #t
                            "::error file=~a,line=1,title=sift format::~a would be reformatted~%"
                            name name))]
                       [else
                          (let ((color?
                                   (color-enabled?
                                      #:no-color
                                      (eq? (config-ref config 'color) 'never))))
                            (display (unified-diff source formatted
                                       #:path (or path "<stdin>")
                                       #:color? color?)
                              (current-output-port)))])
                     1))]
              [write?
                 (write-string-to path formatted)
                 (emit-verbose config "wrote ~a" path)
                 0]
              [diff?
                 ;; `format --diff' is the review-friendly form of `check':
                 ;; the same question, answered with the patch it would make.
                 (display (unified-diff source formatted
                            #:path (or path label "<stdin>")
                            #:color? (color-enabled?
                                        #:no-color (eq? (config-ref config 'color)
                                                     'never)))
                   (current-output-port))
                 (if (string=? formatted source) 0 1)]
              [else
                 (display formatted (current-output-port))
                 0])))))))

(define (command-format args)
  (let* ((state (parse-options args))
         (files (expand-operands (operands state))))
    (when (assq 'help state)
      (display usage-text)
      (exit 0))
    (when (and (assq 'write state) (assq 'stdout state))
      (usage-error "--write and --stdout cannot be used together"))
    (let ((overrides (build-overrides state)))
      (with-config overrides state
        (lambda (config)
          (if (assq 'check state)
            (run-check config files)
            (run-format config files state)))))))

(define (load-effective-config overrides state)
  "Return the effective <config>, or #f when a configuration file is wrong.

The error has already been printed by the time #f comes back, so the caller
only has to answer with the usage status."
  (with-config-error-handler
     (lambda ()
       (call-with-values
         (lambda ()
           (load-config #:cli overrides
             #:config-file (assq-ref state 'config)
             #:trust (and (assq 'trust-config state) #t)))
         (lambda (config layers) config)))))

(define* (with-config overrides state thunk)
  "Run @var{thunk} on the effective configuration, or answer the status 2."
  (let ((config (load-effective-config overrides state)))
    (if config (thunk config) 2)))

(define (run-format config files state)
  (cond
    [(assq 'stdin state)
     (when (> (length files) 1)
       (usage-error "--stdin reads one source; at most one file may be named, as a label"))
     (let ((source (get-string-all (current-input-port)))
           (label (or (assq-ref state 'label) (and (pair? files) (car files)))))
       (if (assq 'write state)
         (if (null? files)
           (usage-error "--write with --stdin needs a real file path to write")
           (format-one config (car files) source #:write? #t))
         (format-one config #f source #:label label
           #:diff? (and (assq 'diff state) #t))))]
    [(null? files)
     ;; No files and no --stdin: read standard input, as every Unix filter does.
     (format-one config #f (get-string-all (current-input-port)))]
    [(assq 'write state)
     (fold (lambda (path status)
             (if (= status 2)
               status
               (call-with-values (lambda () (safe-read-file path))
                 (lambda (source failure)
                   (if failure failure
                     (format-one config path source #:write? #t))))))
       0 files)]
    [else
       (fold (lambda (path status)
               (if (= status 2)
                 status
                 (call-with-values (lambda () (safe-read-file path))
                   (lambda (source failure)
                     (if failure failure
                       (format-one config path source
                         #:diff? (and (assq 'diff state) #t)))))))
         0 files)]))

;;; ===== check =====

(define* (run-check config files #:key (stdin? #f))
  (if (or stdin? (null? files))
    (let ((source (get-string-all (current-input-port))))
      (format-one config #f source #:check? #t))
    (fold (lambda (path status)
            (if (= status 2)
              status
              (call-with-values (lambda () (safe-read-file path))
                (lambda (source failure)
                  (if failure failure
                    (let ((this (format-one config path source #:check? #t)))
                      (max status this)))))))
      0 files)))

(define (command-check args)
  (let* ((state (parse-options args))
         (files (expand-operands (operands state))))
    (when (assq 'help state)
      (display usage-text)
      (exit 0))
    (let ((overrides (build-overrides state)))
      (with-config overrides state
        (lambda (config)
          (run-check config files #:stdin? (and (assq 'stdin state) #t)))))))

;;; ===== lint =====

(define (validate-checker-names! overrides)
  (for-each
     (lambda (key)
       (for-each (lambda (name)
                   (unless (find-checker name)
                     (usage-error "unknown checker `~a'; run `sift list-checkers' to see them all"
                       name)))
         (or (assq-ref overrides key) '())))
     '(checkers exclude-checkers)))

(define (lint-options config)
  (list (cons 'line-length (config-ref config 'line-length))
    (cons 'indent (config-ref config 'indent))
    (cons 'loop-keywords (config-ref config 'loop-keywords))
    (cons 'brackets (config-ref config 'brackets))
    (cons 'max-depth (config-ref config 'max-depth))
    (cons 'max-elements (config-ref config 'max-elements))
    (cons 'max-positional (config-ref config 'max-positional))))

(define* (lint-text config source #:key (path #f) (label #f))
  "Lint one source, returning @code{(values warnings dialect status)}."
  (let* ((dialect (dialect-for config path source #:label label))
         (warnings (lint-source source #:dialect dialect #:file path
                     #:checkers (config-ref config 'checkers)
                     #:exclude-checkers (config-ref config 'exclude-checkers)
                     #:severity (config-ref config 'severity)
                     #:options (lint-options config))))
    (values warnings dialect 0)))

(define* (report-warnings config warnings #:key (files 1))
  "Write @var{warnings} in the configured format, and the summary line."
  (let* ((color? (case [config-ref config 'color]
                   [(always) #t]
                   [(never) #f]
                   [else (color-enabled?)]))
         (format* (config-ref config 'format)))
    (render-warnings warnings #:format format* #:color? color? #:quiet #t)
    (when (and (not (config-ref config 'quiet))
            (memq format* '(human)))
      (let ((errors (count (lambda (warning)
                             (eq? (lint-warning-severity warning) 'error))
                      warnings))
            (warnings-count (count (lambda (warning)
                                     (eq? (lint-warning-severity warning) 'warning))
                              warnings)))
        (render-summary files warnings-count errors 0 #:color? color?)))))

(define (findings? warnings)
  (pair? warnings))

(define* (run-lint config files state)
  (let ((fix? (assq 'fix state)))
    (if (or (assq 'stdin state) (null? files))
      (let* ((label (or (assq-ref state 'label) (and (pair? files) (car files))))
             (source (get-string-all (current-input-port))))
        (call-with-values (lambda () (lint-text config source #:path label))
          (lambda (warnings dialect status)
            (if fix?
              ;; `--fix' with `--stdin' has nowhere to write but stdout: the
              ;; fixed text goes there, and what is left is reported on stderr
              ;; so that an editor can take the buffer and a human can still
              ;; read the reason.
              (let* ((fixed (apply-fixes source warnings))
                     (remaining (call-with-values
                                  (lambda () (lint-text config fixed #:path label))
                                  (lambda (left dialect status) left))))
                (display fixed (current-output-port))
                (render-warnings remaining #:format (config-ref config 'format)
                  #:quiet #t #:port (current-error-port))
                (if (findings? remaining) 1 0))
              (begin
                (report-warnings config warnings #:files 1)
                (max status (if (findings? warnings) 1 0)))))))
      ;; the status of the *run* is the worst status any file produced, so
      ;; the accumulator is what carries it from one file to the next
      (fold (lambda (path run-status)
              (if (= run-status 2)
                run-status
                (call-with-values (lambda () (safe-read-file path))
                  (lambda (source failure)
                    (if failure failure
                      (call-with-values
                        (lambda () (lint-text config source #:path path))
                        (lambda (warnings dialect file-status)
                          (cond
                            [fix?
                               (max run-status
                                 (lint-with-fixes config path source warnings))]
                            [else
                               (report-warnings config warnings #:files 1)
                               (max run-status
                                 (max file-status
                                   (if (findings? warnings) 1 0)))]))))))))
        0
        files))))

(define* (lint-with-fixes config path source warnings)
  "Apply the fixes in @var{warnings} to @var{source}, then lint once more.

A second pass is the whole of the fix loop: after applying the fixes, what
is left is reported, and sift does not try again.  A fixer that keeps
finding work is a bug, not a queue."
  (let* ((fixed (apply-fixes source warnings))
         (changed? (not (string=? fixed source))))
    (when changed?
      (write-string-to path fixed)
      (emit-verbose config "applied fixes to ~a" path))
    (call-with-values (lambda () (lint-text config fixed #:path path))
      (lambda (remaining dialect status)
        (report-warnings config remaining #:files 1)
        ;; A run that fixed everything it found is a clean run: what is left
        ;; is what the exit status talks about, so `lint --fix' can sit at the
        ;; head of a pipeline without failing it.
        (if (findings? remaining) 1 0)))))

(define (command-lint args)
  (let* ((state (parse-options args))
         (files (expand-operands (operands state))))
    (when (assq 'help state)
      (display usage-text)
      (exit 0))
    (let ((overrides (build-overrides state)))
      (with-config overrides state
        (lambda (config)
          (validate-checker-names!
             (list (cons 'checkers (config-ref config 'checkers))
               (cons 'exclude-checkers
                 (config-ref config 'exclude-checkers))))
          (run-lint config files state))))))

;;; ===== ci =====

(define (command-ci args)
  "Check the formatting and lint every file, and say whether the tree is clean.

This is the one command a pipeline needs: it asks both questions, prints
both answers in the chosen format and exits 1 if either of them found
something.  It never writes and never fixes, because a CI job that edits
the tree it is judging is a CI job nobody trusts."
  (let* ((state (parse-options args))
         (files (expand-operands (operands state))))
    (when (assq 'help state)
      (display usage-text)
      (exit 0))
    (when (assq 'fix state)
      (usage-error "ci never fixes; run `sift fix' and commit the result"))
    (when (assq 'write state)
      (usage-error "ci never writes; run `sift format --write' and commit the result"))
    (let ((overrides (build-overrides state)))
      (with-config overrides state
        (lambda (config)
          (let* ((stdin? (and (assq 'stdin state) #t))
                 (check-status (run-check config files #:stdin? stdin?))
                 (lint-status (run-lint config files state)))
            (cond
              [(or (= check-status 2) (= lint-status 2)) 2]
              [(or (= check-status 1) (= lint-status 1)) 1]
              [else
                 (when (and (not (config-ref config 'quiet))
                         (eq? (config-ref config 'format) 'human))
                   (format (current-error-port) "sift: formatting and lint are clean~%"))
                 0])))))))

;;; ===== install-hooks =====

(define (parent-directory directory)
  "Return the parent of @var{directory}, or @code{#f} at the root."
  (let ((parent (dirname directory)))
    (if (string=? parent directory) #f parent)))

(define* (find-git-hooks-directory #:optional (directory (getcwd)))
  "Return the hooks directory of the repository containing @var{directory}.

A worktree has a `.git' *file* holding `gitdir: <path>', so both shapes are
followed; nothing is assumed about where the repository lives."
  (let loop ((directory directory))
    (and directory
      (let ((dot-git (string-append directory "/.git")))
        (cond
          [(file-exists? dot-git)
           (if (eq? (stat:type (stat dot-git)) 'directory)
             (string-append dot-git "/hooks")
             (let ((line (call-with-input-file dot-git read-line)))
               (and (string-prefix? "gitdir:" line)
                 (string-append (string-trim (substring line 7))
                   "/hooks"))))]
          [else (loop (parent-directory directory))])))))

(define hook-script
  (string-append
     "#!/bin/sh\n"
     "# installed by `sift install-hooks'; remove this file to stop it\n"
     "# running, or set SIFT to the program you want it to run.\n"
     "sift=${SIFT:-sift}\n"
     "exec \"$sift\" ci\n"))

(define (command-install-hooks args)
  "Install a pre-commit hook that runs `sift ci'."
  (let* ((state (parse-options args))
         (hooks (find-git-hooks-directory)))
    (unless hooks
      (usage-error "no git repository here; run this inside one"))
    (let ((path (string-append hooks "/pre-commit")))
      (when (and (file-exists? path) (not (assq 'force state)))
        (usage-error "~a already exists; move it away, or pass --force to replace it"
          path))
      (unless (file-exists? hooks) (make-directories hooks))
      (call-with-output-file path
        (lambda (port) (display hook-script port)))
      (chmod path #o755)
      (format #t "installed ~a~%" path)
      0)))

;;; ===== doctor =====

(define (library-directory)
  "Return the directory the sift modules were loaded from, if it can be told."
  (or (find (lambda (directory)
              (file-exists? (string-append directory "/sift/cli.scm")))
        %load-path)
    "unknown"))

(define* (command-doctor args)
  "Say what sift this is, what it found to configure itself, and what it decided.

Adopting a tool is mostly asking why it is doing what it is doing: which
executable ran, which configuration files were found in which order, which
settings a layer won, and whether a Scheme configuration is sitting there
untrusted.  That is what this prints."
  (let* ((state (parse-options args))
         (overrides (build-overrides state)))
    (with-config overrides state
      (lambda (config)
        (let* ((layers (discover-config-files #:cwd (getcwd)))
               (rules (all-checkers))
               (changed (filter (lambda (entry)
                                  (not (equal? (cdr entry) "default")))
                          (config-provenance->alist config))))
          (format #t "sift ~a~%" sift-version)
          (format #t "  guile       ~a~%" (version))
          (format #t "  library     ~a~%" (library-directory))
          (format #t "  rules       ~a~%" (length rules))
          (format #t "  dialects    scheme, elisp, cl~%")
          (newline)
          (format #t "Configuration~%")
          (if (null? layers)
            (format #t "  no file found; the defaults are in effect~%")
            (for-each (lambda (layer)
                        (let* ((kind (car layer))
                               (path (cdr layer))
                               (trusted?
                                  (or (not (scheme-config? path))
                                    (trusted-config-file? path))))
                          (format #t "  ~a  ~a~a~%" kind path
                            (if trusted? "" "  (not trusted: not loaded)"))))
              layers))
          (newline)
          (format #t "Settings that are not the default~%")
          (if (null? changed)
            (format #t "  none~%")
            (for-each (lambda (entry)
                        (format #t "  ~a: ~s  # ~a~%"
                          (car entry) (config-ref config (car entry))
                          (cdr entry)))
              changed))
          (newline)
          (format #t "Try `sift ci .' to check a tree, or `sift explain RULE' to read a rule.~%")
          0)))))

;;; ===== list-checkers, explain, version =====

(define (command-list-checkers args)
  (let* ((state (parse-options args))
         (dialect-name (assq-ref state 'dialect))
         (dialect (and dialect-name
                    (or (string->dialect dialect-name)
                      (usage-error "unknown dialect `~a'" dialect-name))))
         (checkers (if dialect
                     (checkers-for-dialect dialect)
                     (all-checkers))))
    (list-checkers-table checkers)
    0))

(define (command-explain args)
  (let ((state (parse-options args)))
    (match (operands state)
      (() (usage-error "explain needs a checker name"))
      ((name . _)
       (let ((checker (find-checker name)))
         (if checker
           (begin (explain-checker checker) 0)
           (begin
             (format (current-error-port)
               "sift: unknown checker `~a'; run `sift list-checkers' to see them all~%"
               name)
             2)))))))

(define (command-version args)
  (format #t "sift ~a~%" sift-version)
  0)

;;; ===== config subcommand =====

(define (command-config args)
  (match args
    (() (usage-error "config needs a subcommand: show, explain or trust"))
    ((sub . rest)
     (let* ((state (parse-options rest))
            (files (operands state)))
       (case [string->symbol sub]
         [(show)
          (let* ((format-name (or (assq-ref state 'format) "yaml"))
                 (output-format (cond
                                  [(member format-name '("yaml" "yml")) 'yaml]
                                  [(string=? format-name "scheme") 'scheme]
                                  [else
                                     (usage-error
                                        "config show --format is yaml or scheme")]))
                 ;; `--format' means "how to render the configuration" for
                 ;; this subcommand, so it is taken out before the rest of
                 ;; the command line is turned into settings
                 (overrides (build-overrides
                               (remove (lambda (entry)
                                         (eq? (car entry) 'format))
                                 state))))
            (with-config overrides state
              (lambda (config)
                (config-show config #:output-format output-format
                  #:source? (and (assq 'verbose state) #t)
                  #:port (current-output-port))
                0)))]
         [(explain)
          (match files
            (() (usage-error "config explain needs a key"))
            ((key . _)
             (if (config-explain (string->symbol key))
               0
               (begin
                 (format (current-error-port)
                   "sift: unknown configuration key `~a'~%" key)
                 2))))]
         [(trust)
          (match files
            (() (usage-error "config trust needs a file"))
            ((path . _)
             (if (file-exists? path)
               (begin (trust-config-file! path) 0)
               (begin
                 (format (current-error-port) "sift: no such file: ~a~%" path)
                 2))))]
         [else (usage-error "unknown config subcommand `~a'" sub)])))))

;;; ===== Dispatch =====

(define (dispatch args)
  (match args
    (() (usage-error "no command given"))
    ((command . rest)
     (case [string->symbol command]
       [(format) (command-format rest)]
       [(lint) (command-lint rest)]
       [(check) (command-check rest)]
       [(list-checkers) (command-list-checkers rest)]
       [(explain) (command-explain rest)]
       [(config) (command-config rest)]
       [(fix) (command-lint (cons "--fix" rest))]
       [(ci) (command-ci rest)]
       [(doctor) (command-doctor rest)]
       [(install-hooks) (command-install-hooks rest)]
       [(version --version -V) (command-version rest)]
       [(help --help -h) (display usage-text) 0]
       [else (usage-error "unknown command `~a'" command)]))))

(define* (sift-main args)
  "Run sift with @var{args} and return the exit status.

The status is the contract: 0 clean, 1 findings or differences, 2 usage
error, unreadable file, or a file that cannot be parsed."
  (catch 'sift-usage-error
    (lambda () (dispatch args))
    (lambda (key message)
      (format (current-error-port) "sift: ~a~%" message)
      (format (current-error-port) "~a" usage-text)
      2)))
