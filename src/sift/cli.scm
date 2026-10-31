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

(define (long-names option)
  "Return the long names of @var{option}: every name that is a string."
  (filter string? (option-names option)))

;;; The two lists below are read off the specification above rather than
;;; written out again, because a list written out again is a list that drifts.
;;; `--brackets square' and `--label x' were rejected as unrecognized options
;;; for as long as they were missing from a copy of this list, and no test
;;; could tell that they had ever been there.

;; Every long spelling of an option; abbreviations are resolved against these.
(define long-option-names
  (append-map long-names option-specification))

;; The long options that take an argument on the command line.
(define long-options-with-values
  (append-map long-names
    (filter option-required-arg? option-specification)))

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

;;; Each procedure below reads one option's argument and answers with the
;;; value to override, or raises a usage error naming the option, what it
;;; takes and what it got.  They are procedures of the value alone, because
;;; that is what makes the table under them a table: adding an option is one
;;; procedure and one entry, not another twelve lines in the middle of a
;;; `let'.

(define (dialect-option value)
  (or (string->dialect value)
    (usage-error "unknown dialect `~a'; supported dialects are scheme, elisp and cl"
      value)))

(define (style-option value)
  (or (string->style value)
    (usage-error "unknown style `~a'; supported styles are community and dangling"
      value)))

(define (severity-option value)
  (or (string->severity value)
    (usage-error "unknown severity `~a'; sift knows error, warning and info"
      value)))

(define (brackets-option value)
  (or (string->brackets value)
    (usage-error "unknown bracket style `~a'; sift knows square, parens and preserve"
      value)))

(define (format-option value)
  (let ((format (string->symbol value)))
    (if (memq format '(human json null github sarif))
      format
      (usage-error "unknown format `~a'; sift knows human, json, null, github and sarif"
        value))))

(define (positive-integer-option what)
  "Return a reader that accepts a positive integer, or reports @var{what}."
  (lambda (value)
    (let ((n (string->number value)))
      (if (and n (integer? n) (> n 0))
        n
        (usage-error "~a must be a positive integer, got `~a'" what value)))))

;; The options whose argument is a single value, in the order they are read.
(define option-readers
  `((dialect . ,dialect-option)
    (style . ,style-option)
    (severity . ,severity-option)
    (format . ,format-option)
    (brackets . ,brackets-option)
    (line-length . ,(positive-integer-option "--line-length"))
    (indent . ,(positive-integer-option "--indent"))))

;; The options that are a switch on the command line and a setting in the
;; configuration.
(define option-flags
  '(comment-align quiet verbose force))

(define (checker-override state key what)
  "Return the reading of the repeated option @var{key}, or @code{#f}.

`--checker' may be given more than once, and the names inside one giving may
be separated by commas or spaces, so the state holds a list of spellings
that has to be flattened, split, trimmed and de-duplicated before it is a
setting."
  (let ((given (assq-ref state key)))
    (and given
      (cons key
        (delete-duplicates
           (append-map (lambda (item) (checker-name-list item what))
             (reverse given)))))))

(define* (build-overrides state)
  "Return the alist of settings the command line overrides.

Invalid values are usage errors, not silent fallbacks: a `--line-length' of
`abc' means the run is wrong, and saying so is the only useful answer."
  (define (read-option key reader)
    (let ((given (assq-ref state key)))
      (and given (cons key (reader given)))))
  (filter identity
    (append
       (map (lambda (entry) (read-option (car entry) (cdr entry)))
         option-readers)
       (map (lambda (key) (and (assq key state) (cons key #t))) option-flags)
       (list (read-option 'color string->symbol)
         (checker-override state 'checkers "--checker")
         (checker-override state 'exclude-checkers "--exclude-checker")))))

(define lisp-file-extensions
  '(".scm" ".ss" ".sld" ".sls" ".el" ".lisp" ".cl" ".asd" ".lsp" ".system"))

(define (lisp-file? path)
  (let ((dot (string-rindex path #\.)))
    (and dot (member (substring path dot) lisp-file-extensions) #t)))

(define (entry-type path)
  "Return the type of @var{path}, or @code{#f} when it cannot be asked.

A directory being walked is a directory that is being changed: a socket in
/tmp comes and goes between the listing and the question, and a linter that
raises on that is a linter nobody can point at a home directory."
  (catch #t
    (lambda () (stat:type (stat path)))
    (lambda (key . args) #f)))

(define (directory-names directory)
  "Return the entries of @var{directory}, sorted, or @code{()} when unreadable.

`scandir' answers @code{#f} for a directory it may not read, and a walk that
hands @code{#f} to the next step is a walk that raises instead of saying what
it skipped."
  (catch #t
    (lambda ()
      (or (scandir directory (lambda (name)
                               (not (member name '("." ".." ".git")))))
        '()))
    (lambda (key . args) '())))

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
            (let* ((path (string-append (string-trim-right directory #\/)
                           "/" name))
                   (type (entry-type path)))
              (cond
                [(not type) '()]
                [(eq? type 'directory) (directory-files* path)]
                [(lisp-file? name) (list path)]
                [else '()]))]))
     (sort (directory-names directory) string<?)))

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

(define (errno-in thing)
  "Return the error number in @var{thing}, a Guile system-error argument.

Guile hands a `system-error' over as @code{(subroutine format-string
irritants errno)}, and the error number arrives either bare or wrapped in a
one-element list depending on which layer threw it, so both shapes are read."
  (cond
    [(integer? thing) thing]
    [(and (pair? thing) (integer? (car thing)) (null? (cdr thing))) (car thing)]
    [else #f]))

(define (unreadable-file path condition)
  "Say that @var{path} could not be read, in the words the system used.

A `system-error' carries a *format string* where a message would be, so
printing the second element of it is how a program ends up telling a user
`~A: ~S' instead of which file it could not open and why.  The error number
is turned into a sentence, and the number is the one thing about the failure
that reads the same in every language."
  (let* ((numbers (filter-map errno-in
                    (if (pair? condition) condition '())))
         (reason (if (pair? numbers)
                   (strerror (car numbers))
                   (format #f "~a" condition))))
    (format (current-error-port) "sift: cannot read ~a: ~a~%" path reason)
    2))

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

(define* (format-with config dialect path source)
  "Return @code{(values text errors)} for @var{source} under @var{config}.

One place decides which settings of the configuration the formatter is given,
so that `format', `check' and `ci' cannot disagree about what the formatted
text means."
  (format-source source #:dialect dialect #:file path
    #:style (config-ref config 'style)
    #:indent (config-ref config 'indent)
    #:comment-align (config-ref config 'comment-align)
    #:brackets (config-ref config 'brackets)
    #:indent-rules (config-ref config 'indent-rules)))

(define* (format-one config path source #:key (write? #f) (check? #f)
           (label #f) (diff? #f))
  "Format one source, returning the exit status for it.

@var{path} is @code{#f} for standard input.  Nothing is written unless
@var{write?} asks for it; without it the formatted text goes to stdout, so
`format' is a filter."
  (let* ((dialect (dialect-for config path source #:label label)))
    (emit-verbose config "formatting ~a (~a, ~a)"
      (or path "<stdin>") dialect (config-ref config 'style))
    (let ((denied (deny-on-parse-errors config path source dialect)))
      (if denied
        denied
        (call-with-values
          (lambda () (format-with config dialect path source))
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
          (if (and (assq 'check state)
                (machine-format? (config-ref config 'format)))
            (document-status config files
              #:stdin? (reads-stdin? state files)
              #:label (or (assq-ref state 'label) (and (pair? files) (car files)))
              #:formatting? #t #:lint? #f)
            (if (assq 'check state)
              (run-check config files)
              (run-format config files state))))))))

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

;;; ===== One document per run =====
;;;
;;; `human', `github' and `null' are formats a line at a time can carry: the
;;; first is read by a person, the second turns each finding into an
;;; annotation, and the third says nothing.  `json' and `sarif' are documents,
;;; and a run of two files that wrote two documents would write something no
;;; consumer can read.  For those two the whole run is asked first and
;;; answered once -- which is also why `check' stops answering with a diff
;;; there: a machine wants the finding, and a diff is the human form of the
;;; same answer.

(define (machine-format? format*)
  "Return @code{#t} for a format that is one document per run."
  (memq format* '(json sarif)))

(define (reads-stdin? state files)
  "Return @code{#t} when this run reads its source from standard input.

No files and no `--stdin' means standard input, as it does for every Unix
filter: naming no file is not the empty run, it is the piped one."
  (and (or (assq 'stdin state) (null? files)) #t))

(define (formatting-finding path)
  "Return the finding that says @var{path} is not formatted."
  (make-lint-warning 'format 'error
    "the file is not formatted the way sift would format it"
    (make-source-location* path 1 1 0 0)))

(define* (read-sources files #:key (stdin? #f) (label #f))
  "Return @code{(values pairs status)}: one @code{(path . source)} per file.

Standard input is one pair whose path is the label, which is how an editor
names the buffer it piped in."
  (if stdin?
    (values (list (cons label (get-string-all (current-input-port)))) 0)
    (let loop ((rest files) (pairs '()) (status 0))
      (cond
        [(or (null? rest) (= status 2)) (values (reverse pairs) status)]
        [else
           (call-with-values (lambda () (safe-read-file (car rest)))
             (lambda (source failure)
               (if failure
                 (values (reverse pairs) failure)
                 (loop (cdr rest) (cons (cons (car rest) source) pairs) status))))]))))

(define* (formatting-findings config pairs)
  "Return one finding per source in @var{pairs} that would be formatted differently.

A source sift could not read is not a formatting finding: the reader says what
is wrong with it, and a diff of a file with a missing parenthesis would be a
guess at where the parenthesis goes."
  (append-map
     (lambda (pair)
       (let ((path (car pair))
             (source (cdr pair)))
         (call-with-values
           (lambda ()
             (format-with config (dialect-for config path source) path source))
           (lambda (formatted errors)
             (if (or (pair? errors) (string=? formatted source))
               '()
               (list (formatting-finding path)))))))
     pairs))

(define* (lint-findings config pairs)
  "Return every lint finding of every source in @var{pairs}."
  (append-map
     (lambda (pair)
       (call-with-values
         (lambda () (lint-text config (cdr pair) #:path (car pair)))
         (lambda (warnings dialect status) warnings)))
     pairs))

(define* (report-findings config pairs #:key (lint? #f) (formatting? #t))
  "Render the findings of the whole run once, and answer the exit status.

@var{lint?} and @var{formatting?} say which of the two questions to ask;
`check' asks the second, `lint' the first, and `ci' both -- which is what
makes `ci --format=sarif' one document that holds both answers."
  (let* ((warnings (append (or (and lint? (lint-findings config pairs)) '())
                     (or (and formatting? (formatting-findings config pairs))
                       '())))
         (sorted (sort warnings lint-warning<?)))
    (render-warnings sorted #:format (config-ref config 'format))
    (if (null? sorted) 0 1)))

(define* (document-status config files #:key (stdin? #f) (label #f)
           (lint? #f) (formatting? #t))
  "Answer a run of @var{files} in a format that is one document per run."
  (call-with-values
    (lambda () (read-sources files #:stdin? stdin? #:label label))
    (lambda (pairs status)
      (max status (report-findings config pairs #:lint? lint?
                    #:formatting? formatting?)))))

(define* (run-check config files #:key (stdin? #f) (label #f))
  (if (or stdin? (null? files))
    (let ((source (get-string-all (current-input-port))))
      (format-one config #f source #:check? #t #:label label))
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
          (let ((stdin? (and (assq 'stdin state) #t))
                (label (assq-ref state 'label)))
            (if (machine-format? (config-ref config 'format))
              (document-status config files
                #:stdin? (or stdin? (null? files)) #:label label
                #:formatting? #t #:lint? #f)
              (run-check config files #:stdin? stdin? #:label label))))))))

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
    (cons 'max-positional (config-ref config 'max-positional))
    (cons 'max-definition-depth (config-ref config 'max-definition-depth))))

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
          ;; `--fix' writes to the files it judges, so it stays on the path
          ;; that reports as it goes; a format that is one document per run
          ;; would have to hold what it found before the second pass.
          (if (and (machine-format? (config-ref config 'format))
                (not (assq 'fix state)))
            (document-status config files
              #:stdin? (reads-stdin? state files)
              #:label (or (assq-ref state 'label) (and (pair? files) (car files)))
              #:lint? #t #:formatting? #f)
            (run-lint config files state)))))))

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
                 (label (assq-ref state 'label))
                 (status
                    (if (machine-format? (config-ref config 'format))
                      (document-status config files
                        #:stdin? (or stdin? (null? files)) #:label label
                        #:lint? #t #:formatting? #t)
                      (let ((check-status (run-check config files #:stdin? stdin?
                                            #:label label))
                            (lint-status (run-lint config files state)))
                        (cond
                          [(or (= check-status 2) (= lint-status 2)) 2]
                          [(or (= check-status 1) (= lint-status 1)) 1]
                          [else
                             (when (and (not (config-ref config 'quiet))
                                     (eq? (config-ref config 'format) 'human))
                               (format (current-error-port)
                                 "sift: formatting and lint are clean~%"))
                             0])))))
            status))))))

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
