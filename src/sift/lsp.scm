;; sift --- language server
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A language server that speaks JSON-RPC on standard input and output, which
;; is what makes one integration possible instead of four: Eglot in Emacs,
;; `vim.lsp' in Neovim and any LSP client in VS Code all speak this, so the
;; findings and the fixes are written once, here, and each editor only has to
;; start the process.
;;
;; What it does is what sift already does, moved to where an editor can reach
;; it:
;;
;;   textDocument/didOpen, didChange   lint the buffer, publish the findings
;;   textDocument/codeAction           offer each finding's fix as a quickfix
;;   textDocument/formatting           format the buffer
;;   textDocument/hover                say what the rule under the cursor wants
;;
;; What it does not do is as deliberate: there is no completion, no rename, no
;; workspace symbol search.  sift is a formatter and a linter, and a server
;; that answered every request an editor can make would be a server that lies
;; about half of them.
;;
;; Documents are kept whole and replaced whole (`textDocumentSync' 1).  An
;; incremental edit would mean patching a string by character offsets that the
;; editor computed in UTF-16, and the formatter's promise -- read everything,
;; write everything back -- is only kept if the text it reads is the text the
;; editor has.

(define-module (sift lsp)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift lint)
  #:use-module (sift format)
  #:use-module (sift config)
  #:use-module (sift json)
  #:use-module (sift render)
  #:export (lsp-serve
            lsp-diagnostics
            lsp-code-actions
            lsp-formatting))

;;; ===== The wire =====
;;;
;;; A message is a `Content-Length' header, a blank line, and that many *bytes*
;;; of JSON.  Bytes, not characters: the length counts what goes on the wire,
;;; and a buffer with an accent in it would otherwise carry a wrong length and
;;; stop the conversation at the first non-ASCII character.

(define *no-id* (list 'no-id))

(define (read-line-bytes in)
  "Return the next line of @var{in}, without its line ending, or @code{#f}.

The port is binary, so the bytes are read as they arrive and decoded once the
line is complete."
  (let loop ((bytes '()))
    (let ((byte (get-u8 in)))
      (cond
        [(eof-object? byte)
         (and (pair? bytes) (utf8->string (u8-list->bytevector (reverse bytes))))]
        [(= byte 10) ; newline
         (let ((line (if (and (pair? bytes) (= (car bytes) 13))
                       (cdr bytes)
                       bytes)))
           (utf8->string (u8-list->bytevector (reverse line))))]
        [else (loop (cons byte bytes))]))))

(define (frame-length line)
  "Return the length a @code{Content-Length} header names, or @code{#f}."
  (let ((colon (string-index line #\:)))
    (and colon
      (string-prefix? "Content-Length" line)
      (string->number (string-trim (substring line (+ colon 1)))))))

(define* (read-message in)
  "Read one frame from @var{in}.

Three answers, because the loop has three things to do with them:

  @code{#f}                    the input ended
  @code{(ok . message)}        a message
  @code{(malformed . reason)}  a header without JSON, or JSON that would not
                               parse, or a body shorter than its header said

A malformed frame is not the end of the conversation: the editor is told what
went wrong and the next frame is read, which is what lets a client recover
from sending one bad message."
  (let loop ((length #f))
    (let ((line (read-line-bytes in)))
      (cond
        [(not (string? line)) #f]
        [(string-null? line)
         (cond
           [(not length) (cons 'malformed "a message without a Content-Length")]
           [else
             (let ((bytes (get-bytevector-n in length)))
               (cond
                 [(not (bytevector? bytes))
                  (cons 'malformed "the input ended inside a message")]
                 [(< (bytevector-length bytes) length)
                  (cons 'malformed
                        (format #f "the message was ~a bytes long and ~a arrived"
                                length (bytevector-length bytes)))]
                 [else
                   (catch 'sift-json-error
                          (lambda ()
                            (cons 'ok (json-read (utf8->string bytes) #:path "lsp")))
                          (lambda (key value)
                            (cons 'malformed
                                  (format #f "the message is not JSON: ~a"
                                          ;; the error is (path line column message), and
                                          ;; the sentence is the last of them
                                          (if (pair? value)
                                            (car (last-pair value))
                                            value)))))]))])]
        [else
          (loop (or (frame-length line) length))]))))

(define* (send! out message)
  "Write @var{message} to @var{out} as one framed JSON-RPC message."
  (let* ((body (string->utf8 (json-write message)))
         (header (string->utf8
                  (format #f "Content-Length: ~a\r\n\r\n"
                          (bytevector-length body)))))
    (put-bytevector out header)
    (put-bytevector out body)
    (force-output out)))

(define (field name value)
  "Return the JSON object member @var{name} with @var{value}."
  (cons name value))

;;; ===== Positions =====
;;;
;;; LSP counts lines from zero and columns in UTF-16 code units.  Neither is
;;; what a Scheme string counts, so both are computed here from the character
;;; index the reader gave a warning: one walk of the text, and a character
;;; above U+FFFF counts as the two code units it is.

(define (utf16-width character)
  (if (> (char->integer character) #xFFFF) 2 1))

(define (position-of text index)
  "Return @code{(line . column)} for the character @var{index} of @var{text}."
  (let ((limit (min (max index 0) (string-length text))))
    (let loop ((i 0) (line 0) (column 0))
      (if (>= i limit)
        (cons line column)
        (let ((c (string-ref text i)))
          (if (char=? c #\newline)
            (loop (+ i 1) (+ line 1) 0)
            (loop (+ i 1) line (+ column (utf16-width c)))))))))

(define (index-of-position text line column)
  "Return the character index of @var{line} and @var{column} in @var{text}."
  (let loop ((i 0) (current-line 0) (current-column 0))
    (cond
      [(>= i (string-length text)) i]
      [(and (= current-line line) (>= current-column column)) i]
      [else
        (let ((c (string-ref text i)))
          (if (char=? c #\newline)
            (loop (+ i 1) (+ current-line 1) 0)
            (loop (+ i 1) current-line (+ current-column (utf16-width c)))))])))

(define (json-position text index)
  (let ((position (position-of text index)))
    (json-object
     (field "line" (car position))
     (field "character" (cdr position)))))

(define (json-range text start end)
  "Return the LSP range from @var{start} to @var{end} in @var{text}."
  (json-object
   (field "start" (json-position text start))
   (field "end" (json-position text end))))

(define (index-of-text-position text position)
  "Return the character index of an LSP @var{position} in @var{text}."
  (index-of-position text
                     (json-ref position "line" 0)
                     (json-ref position "character" 0)))

(define (range-of-request text params)
  "Return @code{(start . end)} character indices for a request's range."
  (let ((range (json-ref params "range" (json-object))))
    (cons (index-of-text-position text (json-ref range "start" (json-object)))
          (index-of-text-position text (json-ref range "end" (json-object))))))

(define (document-range text)
  "Return the range that covers all of @var{text}."
  (cons 0 (string-length text)))

;;; ===== Documents =====
;;;
;;; One record per open buffer.  The language identifier the client sent is
;;; kept, because `scheme', `emacs-lisp' and `lisp' are three dialects to sift
;;; and one of them has three spellings.

(define-record-type <document>
  (make-document uri text language-id version dialect)
  document?
  (uri document-uri)
  (text document-text set-document-text!)
  (language-id document-language-id)
  (version document-version set-document-version!)
  (dialect document-dialect))

(define (dialect-of language-id uri)
  "Return the dialect a buffer is read as.

The client's language identifier is the first word; the file's own extension
answers when the identifier is something sift has never seen."
  (or (and language-id (string->dialect language-id))
    (and uri (detect-dialect #:file (strip-uri uri)))
    'scheme))

(define (strip-uri uri)
  "Return the file path a @code{file:} URI names, or @var{uri} unchanged."
  (if (string-prefix? "file://" uri)
    (uri-decode (substring uri 7))
    uri))

(define (uri-decode text)
  "Return @var{text} with @code{%XX} escapes decoded, which is what a path in
a URI is."
  (let ((out (open-output-string)))
    (let loop ((i 0))
      (if (>= i (string-length text))
        (get-output-string out)
        (let ((c (string-ref text i)))
          (if (and (char=? c #\%) (< (+ i 3) (string-length text)))
            (begin
              (display (integer->char
                        (string->number (substring text (+ i 1) (+ i 3)) 16))
                       out)
              (loop (+ i 3)))
            (begin (display c out) (loop (+ i 1)))))))))

;;; ===== Findings =====
;;;
;;; Everything the editor sees is computed from the buffer the editor has, at
;;; the moment it asks: no cache to go stale, and a `codeAction' request
;;; answers about the text it was sent rather than about the text that was
;;; linted when the buffer was last opened.

(define (lint-options-of config)
  (list (cons 'line-length (config-ref config 'line-length))
        (cons 'indent (config-ref config 'indent))
        (cons 'loop-keywords (config-ref config 'loop-keywords))
        (cons 'brackets (config-ref config 'brackets))
        (cons 'max-depth (config-ref config 'max-depth))
        (cons 'max-elements (config-ref config 'max-elements))
        (cons 'max-positional (config-ref config 'max-positional))
        (cons 'max-definition-depth (config-ref config 'max-definition-depth))))

(define* (warnings-for config text #:key (dialect 'scheme) (uri #f))
  "Return the warnings @var{text} earns, as the editor should see them."
  (lint-source text #:dialect dialect #:file (and uri (strip-uri uri))
               #:checkers (config-ref config 'checkers)
               #:exclude-checkers (config-ref config 'exclude-checkers)
               #:severity (config-ref config 'severity)
               #:options (lint-options-of config)))

(define (lsp-severity severity)
  "Return the LSP number for a sift @var{severity}: errors are 1, notes 3."
  (case severity
    [(error) 1]
    [(warning) 2]
    [(info) 3]
    [else 4]))

(define (warning-span warning text)
  "Return @code{(start . end)} character indices for @var{warning}.

A warning that named a range covers exactly that; one that named only a
position covers one character, which is what an editor can highlight."
  (let ((range (lint-warning-range warning))
        (index (source-location-index (lint-warning-location warning))))
    (if range
      range
      (cons index (min (string-length text) (+ index 1))))))

(define (diagnostic-of warning text)
  "Return @var{warning} as an LSP diagnostic about @var{text}."
  (let* ((checker (lint-warning-checker warning))
         (code (or (rule-code checker) (symbol->string checker)))
         (span (warning-span warning text)))
    (json-object
     (field "range" (json-range text (car span) (cdr span)))
     (field "severity" (lsp-severity (lint-warning-severity warning)))
     (field "code" code)
     (field "codeDescription"
            (json-object (field "href" (rule-documentation-uri code))))
     (field "source" "sift")
     (field "message" (lint-warning-message warning)))))

(define (rule-documentation-uri code)
  "Return the page that documents the rule @var{code}."
  (format #f "~a#~a" (sift-rules-page) code))

(define* (lsp-diagnostics config text #:key (dialect 'scheme) (uri #f))
  "Return the diagnostics for @var{text}, as the array an editor expects."
  (list->vector
   (map (lambda (warning) (diagnostic-of warning text))
        (warnings-for config text #:dialect dialect #:uri uri))))

;;; ===== Code actions =====
;;;
;;; A fix is a replacement of a character span, which is exactly a text edit,
;;; so the quickfix an editor offers is sift's own fix and not a second
;;; implementation of it.  The action is computed from the buffer the editor
;;; sent, so what the replacement says is what the buffer says.

(define (warning-overlaps? warning start end text)
  "Return @code{#t} when @var{warning} touches @var{start}..@var{end}."
  (let ((span (warning-span warning text)))
    (and (<= (car span) end) (>= (cdr span) start))))

(define (warning-line warning text)
  (car (position-of text (source-location-index
                          (lint-warning-location warning)))))

(define (select-warnings warnings start end text)
  "Return the warnings a request for @var{start}..@var{end} is about.

An editor that asks with a point -- a range whose two ends are the same -- is
asking about the line the cursor is on, which is what a person means by
\"fix this\"; an editor that asks with a selection is asking about that."
  (let ((touching (filter (lambda (warning)
                            (warning-overlaps? warning start end text))
                          warnings)))
    (if (pair? touching)
      touching
      (let ((line (car (position-of text start))))
        (filter (lambda (warning) (= (warning-line warning text) line))
                warnings)))))

(define (code-action-of warning text uri version)
  "Return @var{warning} as a quickfix code action about @var{text}."
  (let* ((span (warning-span warning text))
         (replacement ((lint-warning-fix warning) text (car span) (cdr span)))
         (code (or (rule-code (lint-warning-checker warning))
                 (symbol->string (lint-warning-checker warning))))
         (title (format #f "sift ~a: ~a" code
                        (lint-warning-message warning))))
    (json-object
     (field "title" title)
     (field "kind" "quickfix")
     (field "diagnostics" (vector (diagnostic-of warning text)))
     (field "edit"
            (json-object
             (field "documentChanges"
                    (vector
                     (json-object
                      (field "textDocument"
                             (json-object
                              (field "uri" uri)
                              (field "version" version)))
                      (field "edits"
                             (vector
                              (json-object
                               (field "range" (json-range text (car span) (cdr span)))
                               (field "newText" replacement))))))))))))

(define* (lsp-code-actions config text start end #:key (dialect 'scheme)
                           (uri #f) (version #f))
  "Return the code actions that apply to @var{start}..@var{end} of @var{text}."
  (let ((warnings (warnings-for config text #:dialect dialect #:uri uri)))
    (list->vector
     (filter-map
      (lambda (warning)
        (and (lint-warning-fix warning)
          (lint-warning-range warning)
          (code-action-of warning text (or uri "") version)))
      (select-warnings warnings start end text)))))

;;; ===== Formatting =====
;;;
;;; One edit for the whole buffer, which is what a formatter's answer is: sift
;;; does not rewrite an expression, so there is nothing smaller to send.

(define* (format-text config text #:key (dialect 'scheme) (uri #f))
  "Return @code{(values formatted errors)} for @var{text}."
  (format-source text #:dialect dialect #:file (and uri (strip-uri uri))
                 #:style (config-ref config 'style)
                 #:indent (config-ref config 'indent)
                 #:comment-align (config-ref config 'comment-align)
                 #:brackets (config-ref config 'brackets)
                 #:indent-rules (config-ref config 'indent-rules)))

(define* (lsp-formatting config text #:key (dialect 'scheme) (uri #f))
  "Return @code{(values edits refusal)} for formatting @var{text}.

@var{edits} is the array an editor applies, empty when the text is already
formatted.  @var{refusal} is a sentence when sift could not format at all --
a buffer with a parse error is left alone, because the formatter never guesses
where a delimiter goes -- and the server shows it, so that a Format Document
which did nothing has a reason attached.

Every other answer here is computed from a buffer sift can read the *whole* of
or not at all; a half-written form is the normal state of a buffer being
typed, and the findings for the part that could be read are still reported."
  (call-with-values (lambda () (format-text config text #:dialect dialect
                                            #:uri uri))
    (lambda (formatted errors)
      (cond
        [(pair? errors)
         (values (vector)
                 (format #f "sift cannot format this buffer: ~a"
                         (lint-warning-message (car errors))))]
        [(string=? formatted text) (values (vector) #f)]
        [else
          (values
           (vector
            (json-object
             (field "range"
                    (json-range text (car (document-range text))
                                (cdr (document-range text))))
             (field "newText" formatted)))
           #f)]))))

;;; ===== The hover =====
;;;
;;; An editor's hover is the smallest possible documentation: the rule's name,
;;; what it wants, and where to read the rest.  It is the same sentence the
;;; SARIF `help' carries, so the two cannot describe a rule differently.

(define (hover-of warning)
  (let* ((checker (lint-warning-checker warning))
         (code (or (rule-code checker) (symbol->string checker))))
    (json-object
     (field "contents"
            (json-object
             (field "kind" "markdown")
             (field "value"
                    (format #f "**sift ~a — ~a**~%~%~a~%~%[Rule ~a](~a)"
                            code (lint-warning-checker warning)
                            (or (find-checker-description checker)
                              (lint-warning-message warning))
                            code (rule-documentation-uri code))))))))

(define (find-checker-description name)
  (let ((checker (find-checker name)))
    (and checker (lint-checker-description checker))))

(define* (lsp-hover config text index #:key (dialect 'scheme) (uri #f))
  "Return the hover for the finding at @var{index}, or @code{#f}."
  (let ((warnings (warnings-for config text #:dialect dialect #:uri uri)))
    (let ((found (find (lambda (warning)
                         (warning-overlaps? warning index index text))
                       warnings)))
      (and found (hover-of found)))))

;;; ===== The server =====
;;;
;;; A language server is a long-lived process inside someone's editor, and the
;;; two properties that matter are that it answers, and that it keeps
;;; answering.  Everything below is written for that: a message that cannot be
;;; parsed is answered with a JSON-RPC error and the loop goes on; a handler
;;; that raises is answered with an internal error and the loop goes on; a
;;; request that arrives before `initialize' is refused by the protocol's own
;;; rule; and a buffer that sift cannot read is a buffer with findings, not a
;;; buffer that stops the server.
;;;
;;; Nothing here writes to standard output except a framed message: an editor
;;; that reads a stray line of prose as the length of the next message stops
;;; talking, and looks like a server that hung.

(define (initialize-result)
  "Return the capabilities sift's server offers."
  (json-object
   (field "capabilities"
          (json-object
           ;; 1 is "full": the editor sends the whole buffer after every change.
           (field "textDocumentSync" 1)
           (field "documentFormattingProvider" #t)
           (field "codeActionProvider"
                  (json-object
                   (field "codeActionKinds" (list->vector (list "quickfix")))
                   (field "resolveProvider" #f)))
           (field "hoverProvider" #t)
           (field "positionEncoding" "utf-16")))
   (field "serverInfo"
          (json-object
           (field "name" "sift")
           (field "version" sift-version)))))

;;; --- the conversation's own bookkeeping ---------------------------------
;;;
;;; Two flags, and both are the protocol's: a request before `initialize' is
;;; answered with -32002, and an `exit' after a `shutdown' ends with status 0
;;; while one without a `shutdown' ends with 1.  An editor uses that second
;;; number to tell a clean goodbye from a server that went away.

(define-record-type <session>
  (make-session)
  session?
  (started? session-started? set-session-started!)
  (shutdown? session-shutdown? set-session-shutdown!))

(define (new-session)
  (let ((session (make-session)))
    (set-session-started! session #f)
    (set-session-shutdown! session #f)
    session))

;;; --- writing ------------------------------------------------------------

(define (reply! out id result)
  "Answer the request @var{id} with @var{result}."
  (send! out (json-object
              (field "jsonrpc" "2.0")
              (field "id" id)
              (field "result" result))))

(define (reply-error! out id code message)
  "Answer the request @var{id} with a JSON-RPC error."
  (send! out (json-object
              (field "jsonrpc" "2.0")
              (field "id" id)
              (field "error"
                     (json-object
                      (field "code" code)
                      (field "message" message))))))

(define (show-message! out type message)
  "Show @var{message} in the editor's own log.

@var{type} is the protocol's: 1 is an error, 2 a warning, 3 information.
A server that cannot do what it was asked says so here, because a request
that answers with nothing looks like a request that worked."
  (send! out
         (json-object
          (field "jsonrpc" "2.0")
          (field "method" "window/showMessage")
          (field "params"
                 (json-object
                  (field "type" type)
                  (field "message" message))))))

(define (publish-diagnostics! out uri version diagnostics)
  "Tell the editor what @var{diagnostics} the document at @var{uri} has."
  (send! out
         (json-object
          (field "jsonrpc" "2.0")
          (field "method" "textDocument/publishDiagnostics")
          (field "params"
                 (json-object
                  (field "uri" uri)
                  (field "version" version)
                  (field "diagnostics" diagnostics))))))

(define* (publish! config out document)
  "Send the diagnostics of @var{document} to the editor.

A buffer that does not parse is a buffer with findings -- the reader's own,
with severity `error` and the code `reader` -- so this works the same way
whether the text is a program or half of one."
  (publish-diagnostics! out
                        (document-uri document)
                        (document-version document)
                        (lsp-diagnostics config
                                         (document-text document)
                                         #:dialect (document-dialect document)
                                         #:uri (document-uri document))))

;;; --- reading a change ---------------------------------------------------
;;;
;;; sift asks for full synchronisation, so a change is the whole buffer; but a
;;; client that sends a range anyway is a client to work with rather than to
;;; refuse, and applying the range correctly costs one function.

(define (apply-change text change)
  "Return @var{text} with one `contentChanges' entry of @var{change} applied."
  (let ((replacement (json-ref change "text")))
    (cond
      [(not (string? replacement)) text]
      [(not (json-ref change "range" #f)) replacement]
      [else
        (let* ((range (json-ref change "range"))
               (start (index-of-text-position text
                                              (json-ref range "start" (json-object))))
               (end (index-of-text-position text
                                            (json-ref range "end" (json-object))))
               (from (min start end))
               (to (max start end)))
          ;; A range past the end of the buffer is a client that has not caught
          ;; up; clamping is the only answer that keeps the text usable.
          (string-append (substring text 0 (min from (string-length text)))
                         replacement
                         (substring text (min to (string-length text)))))])))

(define (apply-changes text changes)
  "Return @var{text} with every entry of @var{changes} applied, in order."
  (let loop ((index 0) (text text))
    (if (>= index (vector-length changes))
      text
      (loop (+ index 1) (apply-change text (vector-ref changes index))))))

;;; --- the handlers -------------------------------------------------------

(define* (handle config session documents message out)
  "Answer one message, and return @code{'exit} or @code{'continue}.

Notifications -- messages without an id -- are answered with nothing at all:
a reply to a notification is a protocol error that some clients report as a
dead server."
  (define (request?)
    (not (eq? (json-ref message "id" *no-id*) *no-id*)))
  (define (params)
    (json-ref message "params" (json-object)))
  (define (document-of)
    (hash-ref documents (json-ref (json-ref (params) "textDocument") "uri" "")))
  (let ((method (json-ref message "method" ""))
        (id (json-ref message "id")))
    (cond
      [(string=? method "initialize")
       (set-session-started! session #t)
       (reply! out id (initialize-result))
       'continue]

      [(string=? method "exit")
       ;; The protocol's exit status: 0 after a shutdown, and 1 without one.
       'exit]

      [(and (not (session-started? session))
         (request?)
         (not (string=? method "shutdown")))
       (reply-error! out id -32002
                     "sift: the server has not been initialized")
       'continue]

      [(string=? method "initialized")
       'continue]

      [(string=? method "shutdown")
       (set-session-shutdown! session #t)
       (reply! out id json-null)
       'continue]

      [(string=? method "textDocument/didOpen")
       (let* ((item (json-ref (params) "textDocument"))
              (uri (json-ref item "uri" ""))
              (language (json-ref item "languageId" #f))
              (document (make-document uri
                                       (json-ref item "text" "")
                                       language
                                       (json-ref item "version" #f)
                                       (dialect-of language uri))))
         (hash-set! documents uri document)
         (publish! config out document)
         'continue)]

      [(string=? method "textDocument/didChange")
       (let ((document (document-of)))
         (when document
           (set-document-text! document
                               (apply-changes (document-text document)
                                              (json-ref (params) "contentChanges" (vector))))
           (set-document-version! document
                                  (json-ref (json-ref (params) "textDocument") "version" #f))
           (publish! config out document))
         'continue)]

      [(string=? method "textDocument/didClose")
       (let ((uri (json-ref (json-ref (params) "textDocument") "uri" "")))
         (hash-remove! documents uri)
         ;; An editor keeps the diagnostics it was last given until it is told
         ;; they are gone, so closing a buffer without saying so leaves red
         ;; underlines in the problem list of a file nobody has open.
         (publish-diagnostics! out uri #f (vector))
         'continue)]

      [(string=? method "textDocument/didSave")
       'continue]

      [(string=? method "textDocument/codeAction")
       (let ((document (document-of)))
         (reply! out id
                 (if document
                   (let ((span (range-of-request (document-text document) (params))))
                     (lsp-code-actions config
                                       (document-text document) (car span) (cdr span)
                                       #:dialect (document-dialect document)
                                       #:uri (document-uri document)
                                       #:version (document-version document)))
                   (vector)))
         'continue)]

      [(string=? method "textDocument/formatting")
       (let ((document (document-of)))
         (if document
           (call-with-values
             (lambda ()
               (lsp-formatting config (document-text document)
                               #:dialect (document-dialect document)
                               #:uri (document-uri document)))
             (lambda (edits refusal)
               ;; Format Document that does nothing with no explanation is the
               ;; worst answer a formatter can give; say why instead.
               (when refusal (show-message! out 1 refusal))
               (reply! out id edits)))
           (reply! out id (vector)))
         'continue)]

      [(string=? method "textDocument/hover")
       (let ((document (document-of)))
         (reply! out id
                 (if document
                   (let* ((text (document-text document))
                          (position (json-ref (params) "position" (json-object)))
                          (index (index-of-text-position text position)))
                     (or (lsp-hover config text index
                                    #:dialect (document-dialect document)
                                    #:uri (document-uri document))
                       json-null))
                   json-null))
         'continue)]

      [(string-prefix? "$/" method)
       ;; A notification about the protocol itself: `$/cancelRequest',
       ;; `$/setTrace'.  There is nothing to answer.
       'continue]

      [(not (request?))
       ;; An unknown notification is a client telling us about something we do
       ;; not model; saying nothing is the whole of the correct answer.
       'continue]

      [else
        (reply-error! out id -32601
                      (format #f "sift does not answer `~a'" method))
        'continue])))

;;; --- one message, and one mistake at a time -----------------------------
;;;
;;; A handler that raises is a bug in sift, not in the editor, and the editor
;;; cannot do anything about it -- but it can be told, and the conversation can
;;; go on.  That is the difference between a server that loses one request and
;;; one that loses the session.

(define (serve-one config session documents message out)
  "Answer @var{message}, turning a raised condition into an error response."
  (let ((id (json-ref message "id" *no-id*)))
    (catch #t
           (lambda () (handle config session documents message out))
           (lambda (key . arguments)
             (let ((reason (format #f "sift: internal error (~a)~@[: ~a~]"
                                   key (and (pair? arguments) (car arguments)))))
               ;; A request is answered with an error response; a notification has
               ;; no id to answer, so the reason goes to the editor's own log.
               (if (eq? id *no-id*)
                 (show-message! out 1 reason)
                 (reply-error! out id -32603 reason)))
             'continue))))

;;; --- the loop -----------------------------------------------------------

(define* (lsp-serve config #:key (in #f) (out #f))
  "Serve one editor until it says `exit'.

@var{in} and @var{out} are the binary ports of the conversation, standard
input and standard output by default.

The return value is the status the protocol asks for: 0 when the editor said
`exit` after a `shutdown`, 1 when it said `exit` without one, and 0 when the
input simply ended, because a closed pipe is an editor that closed the file
rather than a server that failed.

What it never does is stop on a bad message.  A frame that is not JSON is
answered with -32700 and the loop continues, and a handler that raises is
answered with -32603 and the loop continues: a server that dies on one request
makes the next one impossible, and the editor shows a dead server where it
should show a finding."
  (let ((port-in (or in (current-input-port)))
        (port-out (or out (current-output-port)))
        (documents (make-hash-table))
        (session (new-session)))
    (let loop ((status 0))
      (let ((frame (read-message port-in)))
        (cond
          [(not frame) status]
          [(eq? (car frame) 'malformed)
           ;; The id of a message that could not be parsed is not known, and
           ;; the protocol says to answer with null.
           (reply-error! port-out json-null -32700 (cdr frame))
           (loop status)]
          [else
            (let ((outcome (serve-one config session documents (cdr frame)
                                      port-out)))
              (if (eq? outcome 'exit)
                (if (session-shutdown? session) 0 1)
                (loop status)))])))))
