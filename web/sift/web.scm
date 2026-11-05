;; sift --- the face the browser sees
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; sift is a program, and a program is a process: it reads files, it writes
;; files, it exits with a status.  None of that exists in a browser tab.  What
;; does exist is a string going in and a string coming out, and this module is
;; that pair of strings: a request as JSON, an answer as JSON.
;;
;; It lives in web/, beside the page and the compatibility layer, and not in
;; src/sift/.  That is the seam this directory exists to keep: `src/sift/' is
;; the library `bin/sift' is, installed with it and knowing nothing about
;; requests or Wasm, and web/ is everything a browser adds.  The dependency
;; runs one way -- web/ imports the core, the core has never heard of web/ --
;; and tests/web.scm asserts both halves of it.
;;
;; The shape is deliberate and it is the whole reason the WebAssembly build is
;; small work rather than a rewrite:
;;
;;   (web-request "{\"op\":\"lint\",\"source\":\"(if a b)\"}")
;;   => "{\"ok\":true,\"result\":{\"warnings\":[...]}}"
;;
;; One procedure, two strings, no ports, no files, no globals.  An editor calls
;; it on every keystroke; a test calls it with a literal; and a Wasm host calls
;; it through the reflection API without knowing that the language on the other
;; side is Scheme.  Because the boundary is JSON, the same module runs unchanged
;; under Guile -- which is how tests/web.scm checks the browser build without a
;; browser.
;;
;; What is deliberately *not* here: reading a project, discovering a
;; `sift.yaml', walking a directory, or keeping an index.  Those are the
;; language server's answers, and they are answers about a filesystem.  The
;; browser has a buffer, so the buffer is the unit of work.
;;
;; The protocols:
;;
;;   request                        answer
;;   -------                        ------
;;   {"op":"format","source":S}     {"ok":true,"result":{"source":S',"changed":B,"warnings":W}}
;;   {"op":"check", "source":S}     the same as format, which is what `sift check' asks
;;   {"op":"lint",  "source":S}     {"ok":true,"result":{"warnings":W,"counts":C}}
;;   {"op":"fix",   "source":S}     {"ok":true,"result":{"source":S'',"applied":N,"warnings":W}}
;;   {"op":"extract", "source":S,"offset":N,"to":T}
;;                                 {"ok":true,"result":{"source":S',"title":T,"changed":B}}
;;   {"op":"checkers"}              {"ok":true,"result":{"checkers":K,"version":V}}
;;   {"op":"version"}               {"ok":true,"result":{"version":V}}
;;
;; and, for all of them,
;;
;;   {"ok":false,"error":{"kind":K,"message":M}}
;;
;; where `kind' is `request' for a body that is not JSON, `usage' for a member
;; that is the wrong type or names something sift does not have, and `internal'
;; for a bug in sift -- which a browser tab reports rather than dies of.

(define-module (sift web)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-13)
  ;; `guard' is R7RS, so it is in `(scheme base)' and not in `(guile)'; the
  ;; selective import is what keeps the two from colliding over everything
  ;; else they share.  `truncate' is R7RS too, and it is named here for the
  ;; same reason in reverse: Guile's default environment has it and a build
  ;; for a target without one does not, so the library that defines it says
  ;; so at the top of the file rather than at the bottom of a stack trace.
  #:use-module ((scheme base) #:select (guard truncate))
  #:use-module (sift records)
  #:use-module (sift json)
  #:use-module (sift format)
  #:use-module (sift lint)
  #:use-module (sift analysis)
  #:export (web-request
            web-version))

;;; ===== The answer shapes =====

(define (web-version)
  "Return the version of sift this module was compiled from."
  sift-version)

(define (object . fields)
  "Return a JSON object of @var{fields}, @code{(name . value)} pairs."
  (apply json-object fields))

(define (field name value)
  "Return one member of a JSON object."
  (cons name value))

(define (success result)
  "Return the answer that carries @var{result}."
  (object (field "ok" #t) (field "result" result)))

(define (failure kind message)
  "Return the answer that carries a reason instead of a result."
  (object (field "ok" #f)
          (field "error" (object (field "kind" (symbol->string kind))
                                 (field "message" message)))))

;;; ===== Stopping a request that cannot be answered =====
;;;
;;; A request that names a dialect sift does not have, or a member of the wrong
;;; type, is not an error to report with a stack: it is an answer, and the
;;; answer says what was wrong.  Getting from the middle of an option reader to
;;; the top of the request is the interesting part, because the two worlds sift
;;; runs in do not agree on how.
;;;
;;; Guile's `raise' demands an exception object -- in Guile 3, `(raise 42)' is
;;; itself an error -- and the browser build's `raise' loses the key that a
;;; `throw' carries, so neither can deliver "a usage failure with this message"
;;; to a handler in a way the other would accept.  So nothing is raised: the
;;; escape continuation of the request is a parameter, and a failure calls it.
;;; That is R7RS, both worlds support it, and the whole protocol is visible in
;;; the two definitions below.

(define current-escape
  (make-parameter #f))

(define (usage-failure template . args)
  "Stop the request being answered, because it asked for something impossible.

The reason is formatted here, where the pieces are, and handed to the escape
continuation `web-request' installed.  Called outside a request -- from a test,
or from a future caller that forgot to install one -- it is an error, because
silently answering something would be worse."
  (let ((message (apply format #f template args))
        (escape (current-escape)))
    (if escape
      (escape (failure 'usage message))
      (error "sift: a usage failure outside a request:" message))))

(define (warning->json warning)
  "Return @var{warning} as the JSON an editor draws."
  (let* ((location (lint-warning-location warning))
         (name (lint-warning-checker warning))
         (range (lint-warning-range warning)))
    (object
     (field "rule" (symbol->string name))
     (field "code" (or (rule-code name) ""))
     (field "severity" (severity->string (lint-warning-severity warning)))
     (field "message" (lint-warning-message warning))
     (field "line" (source-location-line location))
     (field "column" (source-location-column location))
     (field "offset" (source-location-offset location))
     (field "from" (if range (car range) #f))
     (field "to" (if range (cdr range) #f))
     (field "fixable" (if (and (lint-warning-fix warning) range) #t #f)))))

(define (warnings->json warnings)
  "Return @var{warnings} as a JSON array."
  (list->vector (map warning->json warnings)))

(define (count-severity warnings severity)
  "Return how many of @var{warnings} are of @var{severity}."
  (count (lambda (warning)
           (eq? (lint-warning-severity warning) severity))
         warnings))

(define (counts->json warnings)
  "Return the number of findings at each severity."
  (object (field "error" (count-severity warnings 'error))
          (field "warning" (count-severity warnings 'warning))
          (field "info" (count-severity warnings 'info))))

;;; ===== Reading a request =====

(define (request-member request name default)
  "Return the member @var{name} of @var{request}, or @var{default}."
  (let ((value (json-ref request name 'absent)))
    (if (or (eq? value 'absent) (json-null? value)) default value)))

(define (request-string request name default)
  "Return the string member @var{name}, or @var{default}."
  (let ((value (json-ref request name 'absent)))
    (cond [(or (eq? value 'absent) (json-null? value)) default]
          [(string? value) value]
          [else (usage-failure "the \"~a\" member must be a string" name)])))

(define (request-number request name default)
  "Return the number member @var{name}, or @var{default}."
  (let ((value (json-ref request name 'absent)))
    (cond [(or (eq? value 'absent) (json-null? value)) default]
          [(number? value) (if (exact? value) value (inexact->exact (truncate value)))]
          [else (usage-failure "the \"~a\" member must be a number" name)])))

(define (request-boolean request name default)
  "Return the boolean member @var{name}, or @var{default}."
  (let ((value (json-ref request name 'absent)))
    (cond [(or (eq? value 'absent) (json-null? value)) default]
          [(or (eq? value #t) (eq? value json-true)) #t]
          [(or (eq? value #f) (eq? value json-false)) #f]
          [else (usage-failure "the \"~a\" member must be true or false" name)])))

(define (request-array request name)
  "Return the array member @var{name} as a list, or @code{#f} when absent."
  (let ((value (json-ref request name 'absent)))
    (cond [(or (eq? value 'absent) (json-null? value)) #f]
          [(vector? value) (vector->list value)]
          [else (usage-failure "the \"~a\" member must be an array" name)])))

(define (request-source request)
  "Return the buffer the request is about."
  (let ((source (json-ref request "source" 'absent)))
    (if (string? source)
      source
      (usage-failure "a request needs a \"source\" string"))))

(define (request-dialect request)
  "Return the dialect the request asks to be read as."
  (let ((name (request-string request "dialect" "scheme")))
    (or (string->dialect name)
      (usage-failure "unknown dialect: ~a (the dialects are scheme, elisp and cl)"
                     name))))

(define (request-style request)
  "Return the formatting style the request asks for."
  (let ((name (request-string request "style" "community")))
    (or (string->style name)
      (usage-failure "unknown style: ~a (the styles are community and dangling)"
                     name))))

(define (request-brackets request)
  "Return the bracket style, or @code{#f} to let the dialect decide."
  (let ((name (request-string request "brackets" #f)))
    (and name
      (or (string->brackets name)
        (usage-failure "unknown bracket style: ~a (square, parens or preserve)"
                       name)))))

(define (request-blank-lines request)
  "Return how blank lines are treated."
  (let ((name (request-string request "blankLines" "preserve")))
    (or (string->blank-lines name)
      (usage-failure "unknown blank-lines style: ~a (preserve or one)" name))))

(define (request-severity request)
  "Return the smallest severity the request wants reported."
  (let ((name (request-string request "severity" "info")))
    (or (string->severity name)
      (usage-failure "unknown severity: ~a (the severities are error, warning and info)"
                     name))))

(define (request-indent-rules request)
  "Return the project macros the request says how to indent."
  (let ((rules (request-array request "indentRules")))
    (map (lambda (entry)
           (if (and (vector? entry)
                 (= (vector-length entry) 2)
                 (string? (vector-ref entry 0))
                 (number? (vector-ref entry 1)))
             (cons (string->symbol (vector-ref entry 0))
                   (let ((offset (vector-ref entry 1)))
                     (if (exact? offset) offset (inexact->exact (truncate offset)))))
             (usage-failure
              "an \"indentRules\" entry is a [macro-name, offset] pair")))
         (or rules '()))))

(define (request-names request name)
  "Return the checker names listed in @var{name}, as symbols, or @code{#f}."
  (let ((names (request-array request name)))
    (and names
      (map (lambda (name)
             (if (string? name)
               (string->symbol name)
               (usage-failure "every entry of \"~a\" must be a rule name" name)))
           names))))

(define (json-value->option value)
  "Return a JSON value as the option alist wants it.

A string names a choice rather than being text -- `\"loop-keywords\": \"upcase\"'
is the symbol `upcase' the checker compares against -- so strings become
symbols, and an array of them a list of symbols.  That is the same conversion
the YAML configuration goes through, which is why a setting can move from
`sift.yaml' to a request without being rewritten."
  (cond [(string? value) (string->symbol value)]
        [(vector? value) (map json-value->option (vector->list value))]
        [(or (eq? value json-true) (eq? value #t)) #t]
        [(or (eq? value json-false) (eq? value #f)) #f]
        [else value]))

(define (object->options object)
  "Return a JSON object of settings as the alist the checkers read."
  (map (lambda (entry)
         (cons (string->symbol (car entry))
               (json-value->option (cdr entry))))
       (json-fields object)))

(define (request-options request)
  "Return the alist of settings the checkers and the formatter read.

The members that name a setting directly -- `lineLength' and `indent' -- come
first, and an `options' object can carry any of the rest of them, by the names
`sift.yaml' uses."
  (let* ((options (json-ref request "options" 'absent))
         (line-length (request-member request "lineLength" #f))
         (indent (request-member request "indent" #f)))
    (append
     (if line-length (list (cons 'line-length line-length)) '())
     (if indent (list (cons 'indent indent)) '())
     (if (json-object? options) (object->options options) '()))))

;;; ===== The operations =====

(define (request-format request source dialect)
  "Answer a request to format @var{source}."
  (call-with-values
    (lambda ()
      (format-source source
                     #:dialect dialect
                     #:style (request-style request)
                     #:indent (request-number request "indent"
                                              (default-indent dialect))
                     #:comment-align (request-boolean request "commentAlign" #f)
                     #:brackets (request-brackets request)
                     #:indent-rules (request-indent-rules request)
                     #:blank-lines (request-blank-lines request)))
    (lambda (formatted errors)
      (success
       (object (field "source" formatted)
               (field "changed" (if (string=? formatted source) #f #t))
               (field "warnings" (warnings->json errors)))))))

(define (request-lint request source dialect)
  "Answer a request to lint @var{source}."
  (let* ((severity (request-severity request))
         (warnings (lint-source source
                                #:dialect dialect
                                #:file (request-string request "label" #f)
                                #:checkers (request-names request "checkers")
                                #:exclude-checkers
                                (or (request-names request "excludeCheckers") '())
                                #:severity severity
                                #:options (request-options request))))
    (success (object (field "warnings" (warnings->json warnings))
                     (field "counts" (counts->json warnings))
                     (field "severity" (symbol->string severity))))))

(define (request-fix request source dialect)
  "Answer a request to apply every fix @var{source} has, and report what is left."
  (let* ((warnings (lint-source source
                                #:dialect dialect
                                #:file (request-string request "label" #f)
                                #:severity 'info
                                #:options (request-options request)))
         (fixed (apply-fixes source warnings))
         (applied (count (lambda (warning)
                           (and (lint-warning-fix warning)
                             (lint-warning-range warning)))
                         warnings))
         (remaining (lint-source fixed
                                 #:dialect dialect
                                 #:file (request-string request "label" #f)
                                 #:severity (request-severity request)
                                 #:options (request-options request))))
    (success (object (field "source" fixed)
                     (field "applied" applied)
                     (field "changed" (if (string=? fixed source) #f #t))
                     (field "warnings" (warnings->json remaining))))))

(define (example-field example key)
  "Return @var{key} of a rule's example, or the empty string."
  (let ((cell (and (list? example) (assq key example))))
    (if (and cell (string? (cdr cell))) (cdr cell) "")))

(define (checker->json checker)
  "Return @var{checker} as the JSON a rule list is drawn from.

A checker whose dialects are the symbol `all' -- because the rule is about
whitespace and whitespace is whitespace in every Lisp -- is listed with the
three dialects spelled out, so that a rule browser filters on what the rule
does rather than on how it was written down."
  (let ((declared (lint-checker-dialects checker))
        (example (lint-checker-example checker)))
    (object (field "name" (symbol->string (lint-checker-name checker)))
            (field "code" (or (lint-checker-code checker) ""))
            (field "severity" (severity->string (lint-checker-severity checker)))
            (field "dialects"
                   (list->vector
                    (map dialect->string
                         (if (eq? declared 'all) dialects declared))))
            (field "description" (lint-checker-description checker))
            (field "example" (example-field example 'input))
            (field "expected" (example-field example 'fix)))))

(define (request-checkers request)
  "Answer a request for the rules this build carries."
  (success (object
            (field "version" (web-version))
            (field "checkers"
                   (list->vector (map checker->json (all-checkers)))))))

;;; ===== The questions an editor asks =====
;;;
;;; These are the language server's answers about a *buffer*: hover, jump,
;;; references, highlights, the outline, completion, signature and inlay hints.
;;; They are the same procedures `sift lsp' answers with -- `(sift analysis)',
;;; which both compile -- asked with an empty workspace, because a tab has no
;;; project to index.
;;;
;;; That is the whole difference, and it is worth being exact about: a
;;; definition inside the buffer is found, a definition in a file the tab has
;;; never seen is not, and no answer pretends otherwise.  The shapes are the
;;; language server's shapes (a range is a line and a character, both zero
;;; based), so that an editor can read the same code either way.

;; The URI the answers name, because there is one buffer and this is it.
(define buffer-uri "file:///sift-buffer")

;; A workspace with no root and nothing in it: a tab has no project.  It is
;; made once, because it is never written to -- `new-workspace' takes a
;; filter and a scanner, and neither is passed here.
(define empty-workspace (new-workspace))

;; No other open buffers: the request carries the only one there is.
(define empty-documents (make-hash-table))

(define (request-offset request source)
  "Return the cursor position the request asks about, as a character offset."
  (let ((offset (request-number request "offset" #f)))
    (cond
      [(not offset) (usage-failure "a request for an answer at a place needs an \"offset\"")]
      [(or (< offset 0) (> offset (string-length source)))
       (usage-failure "the offset ~a is outside the buffer" offset)]
      [else offset])))

(define (request-hints-range request source)
  "Return the range the request wants hints for, as two offsets."
  (let ((from (request-number request "from" 0))
        (to (request-number request "to" (string-length source))))
    (list (max 0 (min from (string-length source)))
          (max 0 (min to (string-length source))))))

(define (argument request source dialect)
  "Return the buffer, the dialect and the offset a question is about."
  (let ((source (request-source request)))
    (list source (request-dialect request)
          (request-offset request source))))

(define (request-hover request source dialect index)
  "Answer what the symbol at @var{index} is, or nothing when sift cannot say."
  (let* ((sites (definition-sites empty-workspace empty-documents buffer-uri
                                  source dialect index))
         (site (and (pair? sites) (car sites))))
    (success
     (object (field "hover"
                    (if site
                      (symbol-hover (cadr site) dialect (caddr site))
                      json-null))))))

(define (request-definition request source dialect index)
  "Answer where the symbol at @var{index} is defined -- in this buffer only."
  (success (object (field "definitions"
                          (lsp-definition empty-workspace empty-documents
                                          buffer-uri source dialect index)))))

(define (request-references request source dialect index)
  "Answer the places in the buffer that mention the symbol at @var{index}."
  (success (object (field "references"
                          (lsp-references empty-workspace empty-documents
                                          buffer-uri source dialect index)))))

(define (request-highlight request source dialect index)
  "Answer the mentions of the symbol at @var{index}, in this buffer."
  (success (object (field "highlights"
                          (lsp-document-highlights source dialect index)))))

(define (request-symbols request source dialect)
  "Answer the outline of the buffer."
  (success (object (field "symbols"
                          (lsp-document-symbols source #:dialect dialect)))))

(define (request-completion request source dialect index)
  "Answer the names a caller could mean at @var{index}."
  (success (object (field "items"
                          (completion-items empty-workspace empty-documents
                                            buffer-uri source dialect index)))))

(define (request-signature request source dialect index)
  "Answer the formal list and the docstring of the call at @var{index}."
  (success (object (field "signature"
                          (or (lsp-signature-help empty-workspace empty-documents
                                                  buffer-uri source dialect index)
                            json-null)))))

(define (request-inlay-hints request source dialect from to)
  "Answer the parameter names of the calls between @var{from} and @var{to}."
  (success (object (field "hints"
                          (lsp-inlay-hints empty-workspace empty-documents
                                           buffer-uri source dialect from to)))))

(define (edit-span text edit)
  "Return @code{(START . END)} of one edit of a `WorkspaceEdit' in @var{text}."
  (let ((range (json-ref edit "range" (json-object))))
    (cons (index-of-text-position text (json-ref range "start" (json-object)))
          (index-of-text-position text (json-ref range "end" (json-object))))))

(define (apply-edits text edits)
  "Return @var{text} with @var{edits} applied.

An edit is a range and the text that replaces it.  They are applied from the
end of the buffer backwards, because an offset computed against the original
text stops meaning what it said the moment an earlier edit changes the length."
  (fold (lambda (edit acc)
          (let* ((span (edit-span acc edit))
                 (start (car span))
                 (end (cdr span)))
            (string-append (substring acc 0 start)
                           (or (json-ref edit "newText" "") "")
                           (substring acc end))))
        text
        (sort edits (lambda (a b) (> (car (edit-span text a))
                                     (car (edit-span text b)))))))

(define (edits-of action)
  "Return the edits of @var{action}, whichever shape its `edit' has."
  (let ((edit (json-ref action "edit" (json-object))))
    (let ((changes (json-ref edit "documentChanges" #f)))
      (cond
        [(and (vector? changes) (> (vector-length changes) 0))
         (vector->list (json-ref (vector-ref changes 0) "edits"))]
        [(vector? (json-ref edit "edits" #f))
         (vector->list (json-ref edit "edits"))]
        [else '()]))))

(define (extraction-title-phrase to)
  "Return the words that name the extraction @var{to} asks for."
  (cond
    [(string=? to "procedure") "procedure"]
    [(string=? to "lambda") "lambda"]
    [else "bind"]))

(define (request-extract request source dialect)
  "Answer a request to extract the expression at the caret into a binding.

@var{to} is `let' (the default), `procedure' or `lambda', and it picks among
the actions the language server offers at the same place.  The answer is the
whole rewritten buffer rather than a `WorkspaceEdit', so a page that has one
buffer does not have to learn to apply edits in order -- and a request that
finds nothing to extract says so instead of answering the buffer unchanged."
  (let* ((index (request-offset request source))
         (to (request-string request "to" "let"))
         (phrase (extraction-title-phrase to))
         (actions (extract-actions source dialect index index #:uri buffer-uri))
         (action (find (lambda (candidate)
                         (string-contains (json-ref candidate "title") phrase))
                       actions)))
    (if (not action)
      (usage-failure
       "there is no expression to extract to a ~a at offset ~a" to index)
      (let* ((rewritten (apply-edits source (edits-of action)))
             (title (json-ref action "title")))
        (success (object (field "source" rewritten)
                         (field "title" title)
                         (field "changed"
                                (if (string=? rewritten source) #f #t))))))))

(define (answer-question op request)
  "Answer one of the questions an editor asks about a place in the buffer."
  (let* ((source (request-source request))
         (dialect (request-dialect request))
         (index (and (not (string=? op "symbols"))
                  (not (string=? op "inlayHints"))
                  (request-offset request source))))
    (cond
      [(string=? op "hover") (request-hover request source dialect index)]
      [(string=? op "definition") (request-definition request source dialect index)]
      [(string=? op "references") (request-references request source dialect index)]
      [(string=? op "highlight") (request-highlight request source dialect index)]
      [(string=? op "symbols") (request-symbols request source dialect)]
      [(string=? op "completion") (request-completion request source dialect index)]
      [(string=? op "signature") (request-signature request source dialect index)]
      [else
        (let ((range (request-hints-range request source)))
          (request-inlay-hints request source dialect (car range) (cadr range)))])))

(define question-ops
  '("hover" "definition" "references" "highlight" "symbols" "completion"
    "signature" "inlayHints"))

(define (answer-for request)
  "Return the answer to @var{request}, or stop with a usage failure."
  (if (json-object? request)
    (let ((op (json-ref request "op" 'absent)))
      (cond
        [(not (string? op))
         (usage-failure "a request needs an \"op\" string")]
        [(string=? op "version")
         (success (object (field "version" (web-version))))]
        [(string=? op "checkers") (request-checkers request)]
        [(string=? op "format")
         (request-format request (request-source request) (request-dialect request))]
        [(string=? op "check")
         (request-format request (request-source request) (request-dialect request))]
        [(string=? op "lint")
         (request-lint request (request-source request) (request-dialect request))]
        [(string=? op "fix")
         (request-fix request (request-source request) (request-dialect request))]
        [(string=? op "extract")
         (request-extract request (request-source request) (request-dialect request))]
        [(member op question-ops) (answer-question op request)]
        [else
          (usage-failure
           "unknown op: ~a (format, check, lint, fix, extract, checkers, version, or one of ~a)"
           op
           (string-join question-ops ", "))]))
    (usage-failure "a request is a JSON object")))

(define (read-request request-text)
  "Return @var{request-text} read as JSON, or @code{#f} when it is not JSON.

A request that cannot be read is not an error worth a stack: it is a request
that was not JSON, and the answer to it says so.  Losing the reader's line and
column costs the caller nothing -- whoever wrote the document by hand still has
the document."
  (guard (condition (#t #f))
    (let ((request (json-read request-text)))
      (and (json-object? request) request))))

(define (web-request request-text)
  "Return the JSON answer to @var{request-text}, a JSON document.

Nothing escapes: a request that cannot be read, a member of the wrong type and
a bug in sift all come back as @code{{\"ok\":false,...}}, because a browser tab
that throws on a keystroke is a browser tab that stopped working.  A usage
failure leaves through the escape continuation, a request that is not JSON is
answered from `read-request', and anything else -- a bug -- is caught by
`guard', which is R7RS and not Guile's `catch', which the browser build has in
name only."
  (json-write
   (if (string? request-text)
     (call/cc
       (lambda (escape)
         (parameterize ((current-escape escape))
           (guard (condition
                   (#t (failure 'internal
                                (format #f "sift could not answer: ~a" condition))))
             (let ((request (read-request request-text)))
               (if request
                 (answer-for request)
                 (failure 'request "the request is not a JSON object")))))))
     (failure 'usage "a request is a JSON string"))))
