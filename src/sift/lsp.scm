;; sift --- language server
;;
;; SPDX-License-Identifier: MIT

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
  "Read one message from @var{in}, or @code{#f} when the input has ended."
  (let loop ((length #f))
    (let ((line (read-line-bytes in)))
      (cond
        [(not (string? line)) #f]
        [(string-null? line)
         (and length
           (let ((bytes (get-bytevector-n in length)))
             (and (bytevector? bytes)
               (json-read (utf8->string bytes) #:path "lsp"))))]
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
  "Return the edits that format @var{text}, or an empty array for none.

A buffer with a parse error is left alone: the formatter never guesses where a
delimiter goes, and an editor that replaced the text of a half-written form
would be worse than one that said nothing."
  (call-with-values (lambda () (format-text config text #:dialect dialect
                                 #:uri uri))
    (lambda (formatted errors)
      (if (or (pair? errors) (string=? formatted text))
        (vector)
        (vector
           (json-object
              (field "range"
                (json-range text (car (document-range text))
                  (cdr (document-range text))))
              (field "newText" formatted)))))))

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

(define* (publish! config out document)
  "Send the diagnostics of @var{document} to the editor."
  (send! out
    (json-object
       (field "jsonrpc" "2.0")
       (field "method" "textDocument/publishDiagnostics")
       (field "params"
         (json-object
            (field "uri" (document-uri document))
            (field "version" (document-version document))
            (field "diagnostics"
              (lsp-diagnostics config
                (document-text document)
                #:dialect (document-dialect document)
                #:uri (document-uri document))))))))

(define* (handle config documents message out)
  "Answer one message, and return @code{#t} when the server should keep going.

Notifications -- messages without an id -- are answered with nothing at all:
a reply to a notification is a protocol error that some clients report as a
dead server."
  (define (reply result)
    (send! out (json-object
                  (field "jsonrpc" "2.0")
                  (field "id" (json-ref message "id"))
                  (field "result" result))))
  (define (reply-error code text)
    (send! out (json-object
                  (field "jsonrpc" "2.0")
                  (field "id" (json-ref message "id"))
                  (field "error" (json-object
                                    (field "code" code)
                                    (field "message" text))))))
  (define (request?)
    (not (eq? (json-ref message "id" *no-id*) *no-id*)))
  (define (params)
    (json-ref message "params" (json-object)))
  (define (document-of)
    (let* ((params (params))
           (uri (json-ref (json-ref params "textDocument") "uri" "")))
      (hash-ref documents uri)))
  (let ((method (json-ref message "method" "")))
    (cond
      [(string=? method "initialize")
       (reply (initialize-result))
       #t]
      [(string=? method "initialized")
       #t]
      [(string=? method "shutdown")
       (reply json-null)
       #t]
      [(string=? method "exit")
       #f]
      [(string=? method "textDocument/didOpen")
       (let* ((params (params))
              (item (json-ref params "textDocument"))
              (uri (json-ref item "uri" ""))
              (text (json-ref item "text" ""))
              (language (json-ref item "languageId" #f))
              (document (make-document uri text language
                          (json-ref item "version" #f)
                          (dialect-of language uri))))
         (hash-set! documents uri document)
         (publish! config out document)
         #t)]
      [(string=? method "textDocument/didChange")
       (let* ((document (document-of))
              (changes (json-ref (params) "contentChanges" (vector)))
              (text (and (> (vector-length changes) 0)
                      (json-ref (vector-ref changes 0) "text" #f))))
         (when document
           (when text (set-document-text! document text))
           (set-document-version! document
             (json-ref (json-ref (params) "textDocument") "version" #f))
           (publish! config out document))
         #t)]
      [(string=? method "textDocument/didClose")
       (let* ((params (params))
              (uri (json-ref (json-ref params "textDocument") "uri" "")))
         (hash-remove! documents uri)
         ;; An editor keeps the diagnostics it was last given until it is told
         ;; they are gone, so closing a buffer without saying so leaves red
         ;; underlines in the problem list of a file nobody has open.
         (send! out
           (json-object
              (field "jsonrpc" "2.0")
              (field "method" "textDocument/publishDiagnostics")
              (field "params"
                (json-object (field "uri" uri)
                  (field "diagnostics" (vector))))))
         #t)]
      [(string=? method "textDocument/codeAction")
       (let ((document (document-of)))
         (if document
           (let* ((text (document-text document))
                  (span (range-of-request text (params))))
             (reply (lsp-code-actions config text (car span) (cdr span)
                      #:dialect (document-dialect document)
                      #:uri (document-uri document)
                      #:version (document-version document))))
           (reply (vector)))
         #t)]
      [(string=? method "textDocument/formatting")
       (let ((document (document-of)))
         (reply
            (if document
              (lsp-formatting config (document-text document)
                #:dialect (document-dialect document)
                #:uri (document-uri document))
              (vector)))
         #t)]
      [(string=? method "textDocument/hover")
       (let ((document (document-of)))
         (if document
           (let* ((text (document-text document))
                  (position (json-ref (params) "position"))
                  (index (index-of-position text
                           (json-ref position "line" 0)
                           (json-ref position "character" 0))))
             (reply (or (lsp-hover config text index
                          #:dialect (document-dialect document)
                          #:uri (document-uri document))
                      json-null)))
           (reply json-null))
         #t)]
      [(string-prefix? "$/" method)
       ;; A notification about the protocol itself: `$/cancelRequest',
       ;; `$/setTrace'.  There is nothing to answer.
       #t]
      [(not (request?))
       ;; An unknown notification is a client telling us about something we do
       ;; not model; saying nothing is the whole of the correct answer.
       #t]
      [else
         (reply-error -32601 (format #f "sift does not answer ~a" method))
         #t])))

(define* (lsp-serve config #:key (in #f) (out #f))
  "Serve one editor until it says `exit'.

@var{in} and @var{out} are the binary ports of the conversation, standard
input and standard output by default.  The return value is the status: 0 when
the editor said goodbye, 0 when the input ended without one, because a closed
pipe is an editor that closed the file."
  (let ((port-in (or in (current-input-port)))
        (port-out (or out (current-output-port)))
        (documents (make-hash-table)))
    (let loop ()
      (let ((message (read-message port-in)))
        (when (and message (handle config documents message port-out))
          (loop))))
    0))
