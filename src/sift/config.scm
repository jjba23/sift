;; sift --- configuration
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Configuration is merged from four layers, and a layer further down the
;; list always wins:
;;
;;   defaults  <  system ($(prefix)/etc/sift)  <  user ($XDG_CONFIG_HOME)  <
;;   project (sift.yaml, sift.scm)  <  command line
;;
;; Two formats load into the same <config> record.  YAML is data: it is
;; read, validated and can never run anything, which is why it is the format
;; to check in.  Scheme is code: it is evaluated, so it can register an
;; indent rule for a project's own macros and read an environment variable --
;; and so it is only loaded when the user has said the file is trusted (see
;; `trusted-config-file?').  Neither format is privileged: `config show'
;; renders the effective result in either.
;;
;; Unknown keys are errors rather than warnings.  A typo in `line-length'
;; that is silently ignored is a setting the user believes is in effect.
;;
;; Every value remembers where it came from, which is what
;; `config show --source' prints.

(define-module (sift config)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 eval-string)
  #:use-module (sift records)
  #:use-module (sift format)
  #:use-module (sift yaml)
  #:export (<config>
              make-config
              config?
              config-values
              config-provenance
              config-ref
              config-set
              config->alist
              config-provenance->alist
              config-defaults
              config-keys
              config-key-documented?
              load-config-file
              discover-config-files
              load-config
              config-cli-overrides
              config-show
              config-explain
              trust-config-file!
              trusted-config-file?
              scheme-config?
              config-error?
              config-error->string
              with-config-error-handler
              effective-options))

;;; ===== The record =====

(define-record-type <config>
  (make-config values provenance)
  config?
  ;; An alist of settings, and an alist of the same keys to the place the
  ;; winning value came from.  Keeping the provenance beside the values is
  ;; what makes `config show --source' honest: it cannot drift, because it is
  ;; recorded when the value is chosen, not reconstructed afterwards.
  (values config-values)
  (provenance config-provenance))

(define* (config-ref config key #:optional (default #f))
  "Return the configured value of @var{key}, or @var{default}."
  (let ((cell (assq key (config-values config))))
    (if cell (cdr cell) default)))

(define (config-set config key value provenance)
  "Return @var{config} with @var{key} set to @var{value}.

The new value replaces any earlier one for the same key, so the alist holds
one entry per key and `config-ref' always answers with the winning layer."
  (make-config (acons key value
                 (filter (lambda (entry) (not (eq? (car entry) key)))
                   (config-values config)))
    (acons key provenance
      (filter (lambda (entry) (not (eq? (car entry) key)))
        (config-provenance config)))))

(define (config->alist config)
  "Return the effective settings in schema order, one entry per key."
  (map (lambda (entry) (cons (car entry) (config-ref config (car entry))))
    config-keys))

(define* (config-provenance->alist config)
  "Return where each effective setting came from, in schema order."
  (map (lambda (entry)
         (cons (car entry) (or (assq-ref (config-provenance config)
                                 (car entry))
                             "default")))
    config-keys))

;;; ===== The schema =====
;;;
;;; One entry per key: its type, its default, what it does and the command
;;; line flag that overrides it (#f when the key is config-only).

(define config-keys
  '((dialect
       (type . dialect)
       (default . #f)
       (flag . "--dialect")
       (description . "Dialect to read the file as: scheme, elisp or cl."))
    (style
       (type . style)
       (default . community)
       (flag . "--style")
       (description . "Formatting style: community or dangling."))
    (line-length
       (type . integer)
       (default . #f)
       (flag . "--line-length")
       (description . "Soft line limit; #f means 100, or 80 for Emacs Lisp."))
    (indent
       (type . integer)
       (default . 2)
       (flag . "--indent")
       (description . "Indentation width in columns."))
    (indent-rules
       (type . rules)
       (default . ())
       (flag . #f)
       (description . "Indentation for a project's own macros: (symbol . columns) pairs."))
    (brackets
       (type . brackets)
       (default . #f)
       (flag . "--brackets")
       (description . "Bracket style for clause lists: square, parens or preserve."))
    (max-depth
       (type . integer)
       (default . 6)
       (flag . #f)
       (description . "Depth at which wrap-in-let suggests a binding. Config-only."))
    (max-elements
       (type . integer)
       (default . 20)
       (flag . #f)
       (description . "Size at which wrap-in-let suggests a binding. Config-only."))
    (max-definition-depth
       (type . integer)
       (default . 6)
       (flag . #f)
       (description . "Conditionals deep at which nesting-depth warns. Config-only."))
    (max-positional
       (type . integer)
       (default . 5)
       (flag . #f)
       (description . "Positional arguments before keyword-arguments suggests :key. Config-only."))
    (comment-align
       (type . boolean)
       (default . #f)
       (flag . "--comment-align")
       (description . "Align inline comments in contiguous runs."))
    (checkers
       (type . names)
       (default . #f)
       (flag . "--checker")
       (description . "The checkers to run, or #f for every applicable one."))
    (exclude-checkers
       (type . names)
       (default . ())
       (flag . "--exclude-checker")
       (description . "Checkers to skip."))
    (severity
       (type . severity)
       (default . info)
       (flag . "--severity")
       (description . "Minimum severity to report: error, warning or info."))
    (format
       (type . output-format)
       (default . human)
       (flag . "--format")
       (description . "Output format: human, json or null."))
    (color
       (type . color)
       (default . auto)
       (flag . "--no-color")
       (description . "Colour output: auto (a terminal, without NO_COLOR), always or never."))
    (quiet
       (type . boolean)
       (default . #f)
       (flag . "--quiet")
       (description . "Suppress the summary line."))
    (verbose
       (type . boolean)
       (default . #f)
       (flag . "--verbose")
       (description . "Print per-file processing details."))
    (force
      (type . boolean)
      (default . #f)
      (flag . "--force")
      (description . "Format a file that has parse errors, leaving the broken part verbatim."))
    (loop-keywords
       (type . loop-style)
       (default . either)
       (flag . #f)
       (description . "LOOP keyword style: either, literal or symbolic. Config-only."))))

(define (config-defaults)
  "Return the default configuration as a <config>."
  (make-config (map (lambda (entry) (cons (car entry) (assq-ref (cdr entry) 'default)))
                 config-keys)
    (map (lambda (entry) (cons (car entry) "default"))
      config-keys)))

(define (config-key-documented? key)
  (and (assq key config-keys) #t))

(define (config-key-doc key)
  (cdr (assq key config-keys)))

;;; ===== Validation =====

(define-record-type <config-error>
  (make-config-error path line column message)
  config-error-object?
  (path config-error-path)
  (line config-error-line)
  (column config-error-column)
  (message config-error-message))

(define (config-error? thing)
  "Return @code{#t} for either kind of configuration error.

A YAML file fails in the parser and a Scheme or merged file fails in the
validator; the caller reports both the same way."
  (or (config-error-object? thing) (yaml-error? thing)))

(define* (raise-config-error path line column message)
  (throw 'sift-config-error (make-config-error path line column message)))

(define (config-error->string error)
  "Render a configuration error as file:line:column: message."
  (cond
    [(yaml-error? error)
     (format #f "~a:~a:~a: ~a"
       (yaml-error-path error) (yaml-error-line error)
       (yaml-error-column error) (yaml-error-message error))]
    [(config-error-object? error)
     (format #f "~a:~a:~a: ~a"
       (config-error-path error) (config-error-line error)
       (config-error-column error) (config-error-message error))]
    [else (format #f "~a" error)]))

(define* (with-config-error-handler thunk #:key (port (current-error-port)))
  "Run @var{thunk}, turning a configuration error into a message and #f.

The caller decides the exit status; a malformed configuration is a usage
error, and every caller treats it as one."
  (catch 'sift-config-error
    thunk
    (lambda (key object)
      (format port "sift: ~a~%" (config-error->string object))
      #f)))

(define (names-setting value bad)
  "Return @var{value} as a list of symbols, or raise a configuration error.

A single name and a comma separated list of them are both accepted, because
a one-element setting is written without a list and an error about brackets
would be the wrong thing to say to a user who typed `--checker=a'."
  (cond
    [(not value) '()]
    [(or (string? value) (symbol? value))
     (map string->symbol
       (filter (lambda (part) (not (string-null? part)))
         (string-split (if (symbol? value) (symbol->string value) value)
           #\,)))]
    [(list? value)
     (map (lambda (item)
            (cond [(symbol? item) item]
                  [(string? item) (string->symbol item)]
                  [else (bad "a list of checker names")]))
       value)]
    [else (bad "a list of checker names")]))

(define (dialect-setting value bad)
  (let ((dialect (cond [(not value) #f]
                       [(symbol? value) value]
                       [(string? value) (string->dialect value)]
                       [else #f])))
    (if (or (not value) dialect) dialect (bad "scheme, elisp or cl"))))

(define (style-setting value bad)
  (let ((style (cond [(symbol? value) value]
                     [(string? value) (string->style value)]
                     [else #f])))
    (if style style (bad "community or dangling"))))

(define (integer-setting value bad)
  (if (or (not value) (and (integer? value) (> value 0)))
    value
    (bad "a positive integer")))

(define (boolean-setting value bad)
  (if (boolean? value) value (bad "true or false")))

(define (severity-setting value bad)
  (let ((severity (if (string? value) (string->severity value) value)))
    (if (memq severity '(error warning info)) severity
      (bad "error, warning or info"))))

(define (output-format-setting value bad)
  (let ((value (if (string? value) (string->symbol value) value)))
    (if (memq value '(human json null)) value (bad "human, json or null"))))

(define (color-setting value bad)
  (let ((value (cond [(string? value) (string->symbol value)]
                     [(boolean? value) (if value 'always 'never)]
                     [else value])))
    (if (memq value '(auto always never)) value (bad "auto, always or never"))))

(define (brackets-setting value bad)
  (let ((style (cond [(not value) #f]
                     [(symbol? value) value]
                     [(string? value) (string->brackets value)]
                     [else #f])))
    (if (or (not value) style) style (bad "square, parens or preserve"))))

(define (loop-style-setting value bad)
  (let ((value (if (string? value) (string->symbol value) value)))
    (if (memq value '(either literal symbolic)) value
      (bad "either, literal or symbolic"))))

(define (rules-setting value bad)
  "Return @var{value} as a list of @code{(name . columns)} pairs.

YAML has no pair, so the same rule is written `[with-my-thing, 2]' -- a
two-element list -- while a Scheme configuration writes the dotted pair.
Both are read, because the document says both are how you write one and a
setting that cannot be written in the format the README documents is a
setting that does not exist."
  (define (as-pair item)
    (cond
      [(and (list? item) (= 2 (length item)) (integer? (cadr item))
         (or (symbol? (car item)) (string? (car item))))
       (cons (car item) (cadr item))]
      [(and (pair? item) (integer? (cdr item))
         (or (symbol? (car item)) (string? (car item))))
       item]
      [else #f]))
  (define (as-symbol pair)
    (cons (if (symbol? (car pair)) (car pair) (string->symbol (car pair)))
      (cdr pair)))
  (let ((pairs (and (list? value) (map as-pair value))))
    (if (and pairs (every identity pairs))
      (map as-symbol pairs)
      (bad "a list of (symbol . columns) pairs"))))

;;; The procedure that reads each kind of setting, keyed by the key's type.
;;;
;;; A new kind of setting is a procedure here and an entry in this table, not
;;; another branch in the middle of a `case'.  A type with no entry is passed
;;; through as it was read, which is what lets a checker option take whatever
;;; shape that checker documents.
(define setting-validators
  `((dialect . ,dialect-setting)
    (style . ,style-setting)
    (integer . ,integer-setting)
    (boolean . ,boolean-setting)
    (names . ,names-setting)
    (severity . ,severity-setting)
    (output-format . ,output-format-setting)
    (color . ,color-setting)
    (brackets . ,brackets-setting)
    (loop-style . ,loop-style-setting)
    (rules . ,rules-setting)))

(define* (validate-setting key value #:key path line column)
  "Return @var{value} in its canonical form, or raise a configuration error."
  (define (bad expected)
    (raise-config-error path line column
      (format #f "~a must be ~a, got ~s" key expected value)))
  (unless (config-key-documented? key)
    (raise-config-error path line column
      (format #f "unknown configuration key `~a'" key)))
  (let ((validator (assq-ref setting-validators
                     (assq-ref (config-key-doc key) 'type))))
    (if validator (validator value bad) value)))

;;; ===== Loading =====

(define (read-file path)
  (call-with-input-file path get-string-all))

(define* (load-yaml-config path)
  "Load a YAML configuration file, returning @code{(key . value)} entries.

Each value carries the file, line and column it was written on, so a bad
setting is reported where the user can see it."
  (let* ((source (read-file path))
         (root (yaml->nodes source #:path path)))
    (unless (eq? (vector-ref root 0) 'mapping)
      (raise-config-error path (node-line-of root) 1
        "a configuration file must be a mapping of keys to values"))
    (map (lambda (entry)
           (let* ((key-name (car entry))
                  (node (cdr entry))
                  (key (string->symbol key-name)))
             (unless (memq (vector-ref node 0) '(scalar sequence))
               (raise-config-error path (node-line-of node) (node-column-of node)
                 (format #f "~a must be a scalar or a list" key)))
             (cons key (validate-setting key (node->value node)
                         #:path path
                         #:line (node-line-of node)
                         #:column (node-column-of node)))))
      (vector-ref root 1))))

(define (node-line-of node) (vector-ref node 2))
(define (node-column-of node) (vector-ref node 3))

(define (node->value node)
  (case [vector-ref node 0]
    [(scalar) (vector-ref node 1)]
    [(sequence) (map node->value (vector-ref node 1))]
    [else #f]))

(define* (load-scheme-config path)
  "Evaluate a Scheme configuration file, returning its settings.

The file is code, and is only evaluated when the user has trusted it: a
repository that is cloned and opened should not be able to run anything
merely because sift was pointed at it.  What comes back is the value of the
last form, an alist of settings; anything the file does on the way -- such
as registering an indent rule -- is a side effect the user asked for when
they trusted it."
  (let* ((source (read-file path))
         ;; `eval-string' takes `#:filename' so that a run-time error inside
         ;; the configuration names the file it came from; `apply' is used
         ;; because the compiler cannot see that keyword through the direct
         ;; call and warns about the arity.
         (result (apply eval-string (list source #:filename path))))
    (unless (and (list? result) (every pair? result))
      (raise-config-error path 1 1
        (format #f "~a must evaluate to an alist of settings" path)))
    (map (lambda (entry)
           (let ((key (cond [(symbol? (car entry)) (car entry)]
                            [(string? (car entry)) (string->symbol (car entry))]
                            [else (raise-config-error path 1 1
                                    "a setting name must be a symbol")])))
             (cons key (validate-setting key (cdr entry)
                         #:path path #:line 1 #:column 1))))
      result)))

(define (scheme-config? path)
  (string-suffix? ".scm" path))

;;; ===== Trust =====

(define (make-directories path)
  "Create @var{path} and every missing parent, like `mkdir -p'."
  (unless (or (file-exists? path) (string=? path "/") (string-null? path))
    (make-directories (dirname path))
    (catch 'system-error
      (lambda () (mkdir path))
      (lambda (key . args)
        (unless (file-exists? path) (apply throw key args))))))

(define (user-config-directory)
  (or (getenv "XDG_CONFIG_HOME")
    (string-append (or (getenv "HOME") ".") "/.config")))

(define (trust-file-path)
  (string-append (user-config-directory) "/sift/trusted-projects"))

(define (trusted-projects)
  "Return the directories whose Scheme configuration sift may evaluate."
  (let ((path (trust-file-path)))
    (if (file-exists? path)
      (filter (lambda (line) (not (string-null? line)))
        (map string-trim-both
          (string-split (read-file path) #\newline)))
      '())))

(define* (trusted-config-file? path #:key (explicit #f))
  "Return @code{#t} when sift may evaluate the Scheme file at @var{path}.

Three things can make a Scheme configuration trusted, in order: the user
named the file with `--config'; the environment says so for this run
(SIFT_TRUST_CONFIG=1); or its directory is listed in
@code{~/.config/sift/trusted-projects}, which is what `sift config trust'
writes.  A YAML file is always trusted, because reading data runs nothing."
  (or explicit
    (equal? (getenv "SIFT_TRUST_CONFIG") "1")
    (let ((directory (dirname (if (string-prefix? "/" path)
                                path
                                (string-append (getcwd) "/" path)))))
      (member directory (trusted-projects)))))

(define* (trust-config-file! path #:key (port (current-output-port)))
  "Record @var{path}'s directory as trusted, so its Scheme config may run."
  (let* ((absolute (if (string-prefix? "/" path)
                     path
                     (string-append (getcwd) "/" path)))
         (directory (dirname absolute))
         (known (trusted-projects)))
    (unless (member directory known)
      (let ((target (trust-file-path)))
        (make-directories (dirname target))
        (call-with-output-file target
          (lambda (out)
            (for-each (lambda (line) (display line out) (newline out))
              (cons directory known))))))
    (format port "trusted ~a~%" directory)))

;;; ===== Loading a file =====

(define* (load-config-file path #:key (explicit #f))
  "Load @var{path}, returning a list of @code{(key . value)} settings.

A path that does not exist is not an error here: discovery hands over only
files that do."
  (if (scheme-config? path)
    (if (trusted-config-file? path #:explicit explicit)
      (load-scheme-config path)
      (raise-config-error path 1 1
        (string-append
           (format #f "~a is a Scheme config file, which runs code, " path)
           (format #f "and it is not trusted; run `sift config trust ~a'"
             path))))
    (load-yaml-config path)))

;;; ===== Discovery =====

(define config-file-names
  '("sift.yaml" "sift.yml" ".sift.yaml" ".sift.yml"
    "sift.scm" ".sift.scm"))

(define (existing-config-in directory)
  (let ((name (find (lambda (name)
                      (file-exists? (string-append directory "/" name)))
                config-file-names)))
    (and name (string-append directory "/" name))))

(define (parent-directory directory)
  "Return the parent of @var{directory}, or @code{#f} at the file system root.

`dirname' answers " / " for " / ", so a walk that only took `dirname' would
never end; every walk in this module goes through this instead."
  (let ((parent (dirname directory)))
    (if (string=? parent directory) #f parent)))

(define (repository-root directory)
  "Return the nearest directory at or above @var{directory} holding a .git."
  (let loop ((directory (string-trim-right directory #\/)))
    (cond
      [(not directory) #f]
      [(string-null? directory) #f]
      [(file-exists? (string-append directory "/.git")) directory]
      [else (loop (parent-directory directory))])))

(define* (discover-config-files #:key (cwd (getcwd)))
  "Return the configuration files in effect, lowest precedence first.

The project file is the nearest one walking up from @var{cwd}, and the walk
stops at the repository root: a `sift.yaml' in the parent of a checkout
belongs to a different project and must not be applied to this one."
  (let* ((system (find (lambda (path) (file-exists? path))
                   '("/etc/sift/config.yaml" "/etc/sift/config.yml"
                     "/etc/sift/config.scm")))
         (user (existing-config-in
                  (string-append (user-config-directory) "/sift")))
         (root (repository-root cwd))
         (project
            (let loop ((directory (string-trim-right cwd #\/)))
              (cond
                [(not directory) #f]
                [(string-null? directory) #f]
                ;; `=>' keeps the value the test produced without giving the
                ;; clause a body that would just be the test again
                [(existing-config-in directory) => identity]
                [(and root (string=? directory root)) #f]
                [else (loop (parent-directory directory))]))))
    (filter identity
      (list (and system (cons 'system system))
        (and user (cons 'user user))
        (and project (cons 'project project))))))

;;; ===== Merging =====

(define* (load-config #:key (cwd (getcwd)) (cli '()) (config-file #f)
           (trust #f))
  "Return the effective <config>, and the layers it was built from.

@var{cli} is an alist of options given on the command line; those win over
every file.  A @var{config-file} named explicitly replaces discovery, which
is what a caller who says `--config' means."
  (let* ((layers (if config-file
                   (list (cons 'explicit config-file))
                   (discover-config-files #:cwd cwd)))
         (config (config-defaults))
         (loaded '()))
    (for-each
       (lambda (layer)
         (let* ((kind (car layer))
                (path (cdr layer))
                (settings (load-config-file path
                            #:explicit (memq kind
                                         '(explicit)))))
           (set! loaded (append loaded (list (cons kind path))))
           (for-each (lambda (setting)
                       (set! config
                         (config-set config (car setting) (cdr setting)
                           path)))
             settings)))
       layers)
    (for-each (lambda (setting)
                (set! config
                  (config-set config (car setting) (cdr setting)
                    (format #f "command line (~a)"
                      (or (assq-ref (config-key-doc (car setting)) 'flag)
                        "option")))))
      cli)
    (values config loaded)))

;;; ===== Showing =====

(define* (config-show config #:key (output-format 'yaml) (source? #f)
           (port (current-output-port)))
  "Write the effective configuration in @var{format}, with provenance.

With @var{source?}, every value is followed by where it came from, which is
the question a user actually has when a setting does not do what they
expect."
  (case output-format
    [(scheme)
     (display "(\n" port)
     (for-each
        (lambda (entry)
          (let ((key (car entry)) (value (cdr entry)))
            (format port "  (~a . ~s)" key value)
            (if source?
              (format port "  ; ~a~%"
                (assq-ref (config-provenance->alist config) key))
              (newline port))))
        (config->alist config))
     (display ")\n" port)]
    [else
       (for-each
          (lambda (entry)
            (let ((key (car entry)) (value (cdr entry)))
              (format port "~a: " key)
              (cond [(eq? value #t) (display "true" port)]
                    [(eq? value #f) (display "false" port)]
                    [(null? value) (display "[]" port)]
                    [(list? value)
                     (format port "[~a]" (string-join (map (lambda (item)
                                                             (format #f "~a" item))
                                                        value)
                                           ", "))]
                    [else (format port "~a" value)])
              (when source?
                (format port "   # ~a"
                  (assq-ref (config-provenance->alist config) key)))
              (newline port)))
          (config->alist config))
       (when source?
         (newline port)
         (format port "# provenance: defaults < system < user < project < command line~%"))]))

(define* (config-explain key #:key (port (current-output-port)))
  "Explain one configuration key: what it does, its default and its flag."
  (let ((key (if (symbol? key) key (string->symbol key)))
        (doc (config-key-doc (if (symbol? key) key (string->symbol key)))))
    (when doc
      (begin
        (format port "~a~%" key)
        (format port "  ~a~%" (assq-ref doc 'description))
        (format port "  type:    ~a~%" (assq-ref doc 'type))
        (format port "  default: ~s~%" (assq-ref doc 'default))
        (format port "  flag:    ~a~%"
          (or (assq-ref doc 'flag) "none (config-only)"))
        #t))))

(define* (effective-options config)
  "Return the alist the checkers and the formatter read their knobs from."
  (list (cons 'line-length (config-ref config 'line-length))
    (cons 'indent (config-ref config 'indent))
    (cons 'loop-keywords (config-ref config 'loop-keywords))
    (cons 'indent-rules (config-ref config 'indent-rules))
    (cons 'brackets (config-ref config 'brackets))
    (cons 'max-depth (config-ref config 'max-depth))
    (cons 'max-elements (config-ref config 'max-elements))
    (cons 'max-positional (config-ref config 'max-positional))
    (cons 'max-definition-depth (config-ref config 'max-definition-depth))))
