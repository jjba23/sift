;; sift --- language server tests
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The server is tested by talking to it: a session is a list of messages, the
;; process is fed all of them at once, and what comes back is the framing an
;; editor would read.  A unit test of one handler could pass while the wire was
;; broken, and the wire is the half an editor can see.
;;
;; Most tests write the whole conversation to the process before it starts and
;; read it back in one go, because the transport is a stream and the order is
;; the protocol.  The ones that name a project root cannot do that: the project
;; is indexed beside the requests, so those tests run as a client and wait for
;; the server to say the index is ready -- see `session-when-ready'.

(define-module (tests lsp)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (veritas veritas)
  #:use-module (sift json)
  #:use-module (sift compiler)
  #:use-module (sift analysis)
  #:use-module (sift index)
  #:use-module (tests helpers)
  #:export (spec))

;;; ===== Framing =====

(define (raw text)
  "Return a frame whose body is @var{text}, whatever it is.

A session that never sends an unparseable message never tests the half of the
server that handles one."
  (cons 'raw text))

(define (frame-of value)
  "Return @var{value} as one framed message: JSON, or the text of a raw frame."
  (if (and (pair? value) (eq? (car value) 'raw))
    (frame-text (cdr value))
    (frame-text (json-write value))))

(define (frame-text body)
  "Return @var{body} framed with its length in bytes."
  (string-append "Content-Length: "
                 (number->string (bytevector-length (string->utf8 body)))
                 "\r\n\r\n" body))

(define (byte-index-of bytes pattern start)
  "Return where @var{pattern} begins in @var{bytes} at or after @var{start}."
  (let ((limit (- (bytevector-length bytes) (bytevector-length pattern))))
    (let loop ((index start))
      (and (<= index limit)
        (or (and (let same? ((step 0))
                   (or (= step (bytevector-length pattern))
                     (and (= (bytevector-u8-ref bytes (+ index step))
                             (bytevector-u8-ref pattern step))
                       (same? (+ step 1)))))
              index)
          (loop (+ index 1)))))))

(define (slice bytes start end)
  "Return @var{bytes} between @var{start} and @var{end}, as a bytevector.

`(rnrs bytevectors)' gives `bytevector-copy' one argument, so the window is
made and filled by hand."
  (let ((copy (make-bytevector (- end start))))
    (bytevector-copy! bytes start copy 0 (- end start))
    copy))

(define (unframe text)
  "Return the messages in @var{text}, a run of framed messages.

A `Content-Length' counts bytes, so the run is walked as bytes.  Walking it as
characters would hand the rest of a body -- or the next frame's header -- to
`json-read' the moment a message carried a character wider than one byte."
  (let ((bytes (string->utf8 text))
        (separator (string->utf8 "\r\n\r\n")))
    (let loop ((index 0) (messages '()))
      (let ((header-end (byte-index-of bytes separator index)))
        (if (not header-end)
          (reverse messages)
          (let* ((header (utf8->string (slice bytes index header-end)))
                 (colon (string-index header #\:))
                 (length (string->number
                          (string-trim (substring header (+ colon 1)))))
                 (start (+ header-end 4))
                 (end (min (bytevector-length bytes) (+ start length)))
                 (body (utf8->string (slice bytes start end))))
            (loop end (cons (json-read body) messages))))))))

(define* (session messages #:key (arguments '()))
  "Run `sift lsp' on @var{messages} and return @code{(status . messages)}."
  (let* ((input (apply string-append (map frame-of messages)))
         (result (run-sift (append (list "lsp") arguments) #:stdin input)))
    (cons (car result) (unframe (cadr result)))))

(define (answer-to messages id)
  "Return the message in @var{messages} that answers @var{id}."
  (find (lambda (message) (equal? (json-ref message "id") id)) messages))

(define (notifications messages method)
  "Return every notification in @var{messages} whose method is @var{method}."
  (filter (lambda (message) (equal? (json-ref message "method") method)) messages))

(define (published messages uri)
  "Return the last diagnostics published for @var{uri}."
  (let ((all (filter (lambda (message)
                       (and (equal? (json-ref message "method")
                                    "textDocument/publishDiagnostics")
                         (equal? (json-ref (json-ref message "params") "uri")
                                 uri)))
                     messages)))
    (and (pair? all)
      (json-ref (json-ref (car (reverse all)) "params") "diagnostics"))))

;;; ===== Messages =====

(define buffer-uri "file:///tmp/sift-lsp-test.scm")

;; A second buffer, so that a jump across files has somewhere to land and
;; `references' has more than one file to look in.
(define lib-uri "file:///tmp/sift-lsp-test-lib.scm")
(define lib-source "(define (square x)\n  (* x x))\n")

;; A buffer with a fixable warning, an unfixable one, and trailing whitespace.
(define unclean
  "(define (f x)\n  (if (ready?)   \n      (go)))\n")

(define (field name value)
  (cons name value))

(define (text-document)
  (json-object (field "uri" buffer-uri)))

(define* (open text #:key (version 1) (language "scheme"))
  (json-object
   (field "jsonrpc" "2.0")
   (field "method" "textDocument/didOpen")
   (field "params"
          (json-object
           (field "textDocument"
                  (json-object
                   (field "uri" buffer-uri)
                   (field "languageId" language)
                   (field "version" version)
                   (field "text" text)))))))

(define* (change text #:key (version 2))
  (json-object
   (field "jsonrpc" "2.0")
   (field "method" "textDocument/didChange")
   (field "params"
          (json-object
           (field "textDocument"
                  (json-object
                   (field "uri" buffer-uri)
                   (field "version" version)))
           (field "contentChanges"
                  (vector (json-object (field "text" text))))))))

(define (close)
  (json-object
   (field "jsonrpc" "2.0")
   (field "method" "textDocument/didClose")
   (field "params" (json-object (field "textDocument" (text-document))))))

(define (save)
  "Return a `didSave' message for `buffer-uri'."
  (json-object
   (field "jsonrpc" "2.0")
   (field "method" "textDocument/didSave")
   (field "params" (json-object (field "textDocument" (text-document))))))

(define (configuration-with settings)
  "Return a `didChangeConfiguration' notification carrying @var{settings}.

Eglot sends `eglot-workspace-configuration', which is organized by server, so
what a user writes as `(:sift (:inlay-hints :json-false))' arrives under a
`sift' key."
  (json-object
   (field "jsonrpc" "2.0")
   (field "method" "workspace/didChangeConfiguration")
   (field "params" (json-object (field "settings" settings)))))

(define (inlay-hint-request id line)
  "Return an `inlayHint' request for the whole of @var{line} of the buffer."
  (request id "textDocument/inlayHint"
           (json-object
            (field "textDocument" (text-document))
            (field "range" (range (point line 0) (point line 200))))))

(define* (watched-change uri #:optional (type 2))
  "Return a `workspace/didChangeWatchedFiles' notification for @var{uri}.

2 is the protocol's \"changed\": 1 is created and 3 deleted, and the server
has to survive all three."
  (json-object
   (field "jsonrpc" "2.0")
   (field "method" "workspace/didChangeWatchedFiles")
   (field "params"
          (json-object
           (field "changes"
                  (vector (json-object (field "uri" uri) (field "type" type))))))))

(define (publications messages uri)
  "Return how many times @var{messages} publishes findings for @var{uri}."
  (length (filter (lambda (message)
                    (and (equal? (json-ref message "method")
                                 "textDocument/publishDiagnostics")
                      (equal? (json-ref (json-ref message "params") "uri") uri)))
                  messages)))

(define (exit-message)
  (json-object (field "jsonrpc" "2.0") (field "method" "exit")))

(define (request id method params)
  (json-object
   (field "jsonrpc" "2.0")
   (field "id" id)
   (field "method" method)
   (field "params" params)))

(define (point line character)
  (json-object (field "line" line) (field "character" character)))

(define (range start end)
  (json-object (field "start" start) (field "end" end)))

(define unbalanced
  "(define (f x)\n  (if (ready?)\n")

(define (open-with-text text)
  "Return a `didOpen' message for `buffer-uri' whose text is @var{text}.

A client that sends a number where the protocol says string is a client with a
bug; the server has to survive it."
  (json-object
   (field "jsonrpc" "2.0")
   (field "method" "textDocument/didOpen")
   (field "params"
          (json-object
           (field "textDocument"
                  (json-object
                   (field "uri" buffer-uri)
                   (field "languageId" "scheme")
                   (field "version" 1)
                   (field "text" text)))))))

(define (settings-object pairs)
  "Return an `initializationOptions' object holding @var{pairs} of settings.

The suite turns the standard library off unless a test asks for one: what a
machine happens to have installed is not something a test can assert about, and
a test that read a thousand library files would be a test that takes a minute."
  (json-object
   (field "sift"
          (apply json-object
                 (map (lambda (pair) (field (car pair) (cdr pair))) pairs)))))

(define (initialize-request id)
  (request id "initialize"
           (json-object (field "capabilities" (json-object))
                        (field "initializationOptions"
                               (settings-object
                                (list (cons "stdlib" json-false)
                                      (cons "compiler-diagnostics" json-false)))))))

(define (configuration-changed)
  "Return a `workspace/didChangeConfiguration' notification.

The settings restate the ones `initialize-request' sent, because a client sends
its whole configuration on this notification: an empty object is a client with
no settings, which puts the standard library and the compiler back to their
defaults.  The change re-checks every open buffer, and a compile started for one
of them reports in a thread of its own -- a test counting publications would be
counting a compile as well."
  (configuration-with
   (settings-object
    (list (cons "stdlib" json-false)
          (cons "compiler-diagnostics" json-false)))))

(define* (initialize-with-root id root-uri #:key (stdlib json-false)
                               (compiler json-false))
  "Return an `initialize' message whose workspace is @var{root-uri}.

A client that names a root is asking about a project, not only about the
buffers it has open, and the server answers it from the files on disk; a test
that wants a standard library names the directories of one instead.  The
compiler is off unless a test asks for it: a compile is a process, and a suite
that started one per buffer would be a suite that measured the compilers."
  (request id "initialize"
           (json-object (field "rootUri" root-uri)
                        (field "capabilities" (json-object))
                        (field "initializationOptions"
                               (settings-object
                                (list (cons "stdlib" stdlib)
                                      (cons "compiler-diagnostics" compiler)))))))

(define (initialize-with-compiler id)
  "Return an `initialize' message that asks the compiler for its opinion."
  (request id "initialize"
           (json-object (field "capabilities" (json-object))
                        (field "initializationOptions"
                               (settings-object
                                (list (cons "stdlib" json-false)
                                      (cons "compiler-diagnostics" #t)))))))

(define (with-cache-home directory thunk)
  "Call @var{thunk} with the index cache kept inside @var{directory}.

The cache is a file named after a hash of the workspace root, so a session
with a root writes one; keeping it in the test's own temporary directory means
the test can see it, and that nothing is left in the user's cache afterwards."
  (let ((previous (getenv "XDG_CACHE_HOME")))
    (setenv "XDG_CACHE_HOME" (string-append directory "/cache"))
    (dynamic-wind
      (lambda () #f)
      thunk
      (lambda ()
        (if previous
          (setenv "XDG_CACHE_HOME" previous)
          (unsetenv "XDG_CACHE_HOME"))))))

(define (signature-request id line character)
  (request id "textDocument/signatureHelp"
           (json-object (field "textDocument" (text-document))
                        (field "position" (point line character)))))

(define (completion-request id line character)
  (request id "textDocument/completion"
           (json-object (field "textDocument" (text-document))
                        (field "position" (point line character)))))

(define (code-action-request id line character)
  (request id "textDocument/codeAction"
           (json-object
            (field "textDocument" (text-document))
            (field "range" (range (point line character) (point line character)))
            (field "context" (json-object (field "diagnostics" (vector)))))))

(define (code-action-only-request id line character only)
  ;; A client that asks for `context.only' is asking a narrower question, and
  ;; a server that answered the wider one would tempt an editor into showing
  ;; a rewrite where the user asked for a fix.
  (request id "textDocument/codeAction"
           (json-object
            (field "textDocument" (text-document))
            (field "range" (range (point line character) (point line character)))
            (field "context"
                   (json-object (field "diagnostics" (vector))
                                (field "only" (list->vector only)))))))

(define (formatting-request id)
  (request id "textDocument/formatting"
           (json-object
            (field "textDocument" (text-document))
            (field "options"
                   (json-object (field "tabSize" 2) (field "insertSpaces" #t))))))

(define (hover-request id line character)
  (request id "textDocument/hover"
           (json-object
            (field "textDocument" (text-document))
            (field "position" (point line character)))))

(define (shutdown-request id)
  (request id "shutdown" (json-object)))

;;; ===== Navigation =====
;;;
;;; A jump is asked for with a position, so these builders take a line and a
;;; column rather than a character index: turning the protocol's counts into an
;;; offset is the server's work, and a test that did it first would agree with
;;; a broken implementation.

(define* (open-at uri text #:key (version 1) (language "scheme"))
  (json-object
   (field "jsonrpc" "2.0")
   (field "method" "textDocument/didOpen")
   (field "params"
          (json-object
           (field "textDocument"
                  (json-object
                   (field "uri" uri)
                   (field "languageId" language)
                   (field "version" version)
                   (field "text" text)))))))

(define (position-request id method uri line character)
  (request id method
           (json-object
            (field "textDocument" (json-object (field "uri" uri)))
            (field "position" (point line character)))))

(define* (change-at uri text #:key (version 2))
  "Return a `didChange' for @var{uri} whose whole text is @var{text}."
  (json-object
   (field "jsonrpc" "2.0")
   (field "method" "textDocument/didChange")
   (field "params"
          (json-object
           (field "textDocument"
                  (json-object (field "uri" uri) (field "version" version)))
           (field "contentChanges"
                  (vector (json-object (field "text" text))))))))

(define (definition-request id uri line character)
  (position-request id "textDocument/definition" uri line character))

(define (highlight-request id uri line character)
  (position-request id "textDocument/documentHighlight" uri line character))

(define (hover-at-request id uri line character)
  (position-request id "textDocument/hover" uri line character))

(define (references-request id uri line character)
  (request id "textDocument/references"
           (json-object
            (field "textDocument" (json-object (field "uri" uri)))
            (field "position" (point line character))
            (field "context" (json-object (field "includeDeclaration" #t))))))

(define (symbols-request id uri)
  (request id "textDocument/documentSymbol"
           (json-object (field "textDocument" (json-object (field "uri" uri))))))

(define (completion-at-request id uri line character)
  (position-request id "textDocument/completion" uri line character))

(define (prepare-rename-request id uri line character)
  (position-request id "textDocument/prepareRename" uri line character))

(define (rename-request id uri line character new-name)
  (request id "textDocument/rename"
           (json-object
            (field "textDocument" (json-object (field "uri" uri)))
            (field "position" (point line character))
            (field "newName" new-name))))

(define (rename-file-request id old-uri new-uri)
  (request id "workspace/willRenameFiles"
           (json-object
            (field "files"
                   (vector (json-object (field "oldUri" old-uri)
                                        (field "newUri" new-uri)))))))

(define (workspace-symbol-request id query)
  (request id "workspace/symbol" (json-object (field "query" query))))

;;; ===== A session that waits for the index =====
;;;
;;; A project is read in a thread of its own, so a test that jumps into one
;;; has to be a client rather than a batch: it sends `initialize', reads what
;;; comes back, and carries on when the server says the index is ready.  The
;;; messages that arrive while it waits are returned with the rest, because
;;; the progress it reported is part of what a test can assert about.

(define (header-length line)
  "Return the number a @code{Content-Length} header names, or @code{#f}."
  (let ((colon (string-index line #\:)))
    (and colon
      (string-prefix? "Content-Length" line)
      (string->number (string-trim (string-trim-right
                                    (substring line (+ colon 1)) #\return))))))

(define (read-frame port)
  "Return the next message on @var{port}, or @code{#f} at its end."
  (let loop ((length #f))
    (let ((line (read-line port)))
      (cond
        [(eof-object? line) #f]
        [(string-null? (string-trim-right line #\return))
         ;; A `Content-Length' counts bytes, so the body is read as bytes and
         ;; decoded once: a character wider than one byte would otherwise make
         ;; the frame shorter than the length it announced.
         (and length (json-read (utf8->string (get-bytevector-n port length))))]
        [else (loop (or (header-length line) length))]))))

(define (read-until port done?)
  "Read messages from @var{port} until @var{done?} or the end of the stream."
  (let loop ((messages '()))
    (let ((message (read-frame port)))
      (cond
        [(not message) (reverse messages)]
        [(done? message) (reverse (cons message messages))]
        [else (loop (cons message messages))]))))

(define (index-ready? message)
  (and (equal? (json-ref message "method") "sift/indexStatus")
    (equal? (json-ref (json-ref message "params") "state") "ready")))

(define* (session-when-ready messages #:key (arguments '()))
  "Run `sift lsp' on @var{messages}, waiting for the project to be indexed.

Used by every test that names a workspace root: the index is built beside the
requests rather than before them, so the test asks when it is done exactly the
way an editor learns it -- from @code{sift/indexStatus}.  The notification can
arrive before or after the answer to `initialize', so both are waited for
together and the test gets whatever had already come in."
  (let* ((command (string-append (test-root) "/bin/sift"))
         (port (apply open-pipe* OPEN_BOTH command "lsp" arguments)))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (display (frame-of (car messages)) port)
        (force-output port)
        (let wait ((seen '()))
          (if (and (answer-to seen 1) (any index-ready? seen))
            (begin
              (for-each (lambda (message) (display (frame-of message) port))
                        (cdr messages))
              (display (frame-of (shutdown-request 99)) port)
              (display (frame-of (exit-message)) port)
              (force-output port)
              (cons 0 (append (reverse seen)
                              (read-until port (lambda (message) #f)))))
            (let ((message (read-frame port)))
              (if message
                (wait (cons message seen))
                (cons 0 (reverse seen)))))))
      (lambda ()
        (catch #t (lambda () (close-pipe port)) (lambda (key . args) #f))))))

(define (milliseconds-since start)
  "Return the whole milliseconds between @var{start} and now."
  (quotient (* (- (get-internal-real-time) start) 1000)
            internal-time-units-per-second))

;;; ===== Reading answers =====

(define (diagnostic-code diagnostic) (json-ref diagnostic "code"))
(define (diagnostic-severity diagnostic) (json-ref diagnostic "severity"))

(define (diagnostic-with messages code)
  (let ((diagnostics (published messages buffer-uri)))
    (and diagnostics
      (find (lambda (diagnostic) (equal? code (diagnostic-code diagnostic)))
            (vector->list diagnostics)))))

(define (start-of diagnostic)
  (json-ref (json-ref diagnostic "range") "start"))

(define (spec)
  (suite "language server"
    #:concurrent? #f

    (test "initialize names the capabilities sift offers"
      (let* ((result (session (list (initialize-request 1))))
             (capabilities (json-ref (json-ref (answer-to (cdr result) 1) "result")
                                     "capabilities")))
        (assert-equal #:expect 0 #:got (car result))
        (assert-equal #:expect 1
          #:got (json-ref (json-ref capabilities "textDocumentSync") "change")
          #:name "a change replaces the whole buffer")
        (assert-equal #:expect 'true
          #:got (json-ref (json-ref capabilities "textDocumentSync") "openClose")
          #:name "the server hears about a buffer opening and closing")
        (assert-true
          (json-object? (json-ref (json-ref capabilities "textDocumentSync") "save"))
          #:name "and about a save, which is what refreshes the project")
        (assert-equal #:expect 'true
          #:got (json-ref capabilities "documentFormattingProvider")
          #:name "the server formats")
        (assert-equal #:expect #("quickfix" "refactor.extract"
                                 "refactor.inline" "refactor.rewrite"
                                 "source.fixAll" "source.fixAll.sift"
                                 "source.organizeImports"
                                 "source.optimizeImports.sift"
                                 "source.sortImports.sift")
          #:got (json-ref (json-ref capabilities "codeActionProvider")
                          "codeActionKinds")
          #:name "the server offers fixes, extractions, the one rewrite it can make, and both import operations")
        (assert-equal #:expect 'true
          #:got (json-ref capabilities "hoverProvider")
          #:name "the server answers a hover")))

    (test "opening a buffer publishes its findings"
      (let ((result (session (list (initialize-request 1) (open unclean)))))
        (assert-equal #:expect 0 #:got (car result))
        (let ((diagnostic (diagnostic-with (cdr result) "SIFT012")))
          (assert-true (and diagnostic #t) #:name "the if has no else branch")
          (when diagnostic
            (assert-equal #:expect 2 #:got (diagnostic-severity diagnostic)
              #:name "a warning is a warning")))))

    (test "the compiler's findings are published after the reader's"
      (if (not (compiler-available? 'scheme))
        (assert-true #t #:name "guild is not installed; skipped")
        (let* ((result (session (list (initialize-with-compiler 1)
                                      (open "(define (f x)\n  (unbound-fn x 1))\n"))))
               (diagnostics (published (cdr result) buffer-uri))
               (compiler (and diagnostics
                           (find (lambda (diagnostic)
                                   (equal? (json-ref diagnostic "source") "guild"))
                                 (vector->list diagnostics)))))
          (assert-equal #:expect 0 #:got (car result))
          (assert-true (and compiler #t)
            #:name "a finding from the real compiler arrived")
          (when compiler
            (assert-contains #:expect (json-ref compiler "message")
              #:got "unbound-fn")))))

    (test "a finding carries the range an editor highlights"
      (let* ((result (session (list (initialize-request 1) (open unclean))))
             (diagnostic (diagnostic-with (cdr result) "SIFT012"))
             (start (start-of diagnostic)))
        (assert-equal #:expect 1 #:got (json-ref start "line")
          #:name "the if is on the second line, counted from zero")
        (assert-equal #:expect 3 #:got (json-ref start "character")
          #:name "at the column the `if` itself is in")))

    (test "a column is counted in the units the protocol counts"
      ;; The emoji is one character in a Scheme string and two code units in
      ;; UTF-16, which is what an editor indexes by.
      (let* ((text (string-append "(define (f x)\n  (list \""
                                  (string (integer->char #x1F600)) "\")   \n    0)\n"))
             (result (session (list (initialize-request 1)
                                    (open text))))
             (diagnostic (diagnostic-with (cdr result) "SIFT001")))
        (assert-true (and diagnostic #t)
          #:name "the trailing whitespace was found")
        (when diagnostic
          (assert-equal #:expect 13
            #:got (json-ref (start-of diagnostic) "character")
            #:name "the emoji counted as the two units it is"))))

    (test "a change republishes the findings of the new text"
      (let* ((result (session (list (initialize-request 1)
                                    (open unclean)
                                    (change "(define (f x)\n  (+ x 1))\n"))))
             (diagnostics (published (cdr result) buffer-uri)))
        (assert-equal #:expect 0 #:got (car result))
        (assert-equal #:expect 0 #:got (vector-length diagnostics)
          #:name "the clean text has nothing to report")))

    (test "the same text is not checked twice"
      ;; An editor re-renders every diagnostic it is handed, and the text the
      ;; last report was about is the one thing sift and the editor can agree
      ;; on: a change that carries the same text leaves the findings exactly
      ;; where the editor already has them.
      (let* ((result (session (list (initialize-request 1)
                                    (open unclean)
                                    ;; the same text again: the findings are
                                    ;; the ones the editor already has
                                    (change unclean))))
             (published* (notifications (cdr result)
                                        "textDocument/publishDiagnostics")))
        (assert-equal #:expect 1 #:got (length published*)
          #:name "one buffer, one publication")))

    (test "new text is checked again even when its findings look the same"
      ;; A report is dropped by an editor whose buffer has moved on (Eglot
      ;; ignores a `publishDiagnostics' whose version is not its own), so the
      ;; findings being familiar is not the same thing as the editor having
      ;; them.  The text differing is the question worth asking.
      (let* ((result (session (list (initialize-request 1)
                                    (open unclean)
                                    ;; the same findings, one comment richer
                                    (change (string-append ";; kept\n" unclean)
                                            #:version 2))))
             (diagnostics (published (cdr result) buffer-uri))
             (if-diagnostic (find (lambda (diagnostic)
                                    (equal? "SIFT012" (diagnostic-code diagnostic)))
                                  (vector->list diagnostics))))
        (assert-equal #:expect 2 #:got (publications (cdr result) buffer-uri)
          #:name "the new text was reported")
        (assert-equal #:expect 2 #:got (json-ref (start-of if-diagnostic) "line")
          #:name "and the finding moved with the text around it")))

    (test "a stale whole-file report is not drawn over a fixed buffer"
      ;; The whole-file rules run in a thread of their own, so their report can
      ;; be finished while the message loop is waiting for the next message.
      ;; An edit that arrives then is reported clean, and a pass about the
      ;; text before it would follow that clean report and put the old
      ;; findings back under a cursor that has moved past them.  What the
      ;; editor is left holding is a report about the text it has.
      (let* ((broken (string-append "(define (open-file path mode)\n"
                                    "  (list path mode))\n\n"
                                    "(open-file \"a\")\n"))
             (fixed (string-append "(define (open-file path mode)\n"
                                   "  (list path mode))\n\n"
                                   "(open-file \"a\" \"r\")\n"))
             (result (session (list (initialize-request 1)
                                    (open broken)
                                    (change fixed #:version 2))))
             (diagnostics (published (cdr result) buffer-uri)))
        (assert-equal #:expect 0 #:got (car result))
        (assert-equal #:expect 0 #:got (vector-length diagnostics)
          #:name "the call has its argument, and nothing is left under it")))

    (test "a save asks the server to look at the buffer again"
      ;; A save is the moment the project changed: the index sift answers
      ;; jumps from is now stale, and a finding can be about a file nobody has
      ;; open.  The buffer's own text did not change, which is exactly why
      ;; this is a question the client has to be able to ask.
      (let ((result (session (list (initialize-request 1)
                                   (open unclean)
                                   (save)))))
        (assert-equal #:expect 0 #:got (car result))
        (assert-equal #:expect 2 #:got (publications (cdr result) buffer-uri)
          #:name "the buffer is reported, and reported again after the save")))

    (test "a file changed on disk is a reason to check the open buffers"
      (let ((result (session (list (initialize-request 1)
                                   (open unclean)
                                   (watched-change lib-uri)))))
        (assert-equal #:expect 0 #:got (car result))
        (assert-true (>= (publications (cdr result) buffer-uri) 1)
          #:name "the open buffer is still answered for")))

    (test "the settings are read again when the client says they changed"
      (let* ((result (session (list (initialize-request 1)
                                    (open unclean)
                                    (configuration-changed))))
             (logs (notifications (cdr result) "window/logMessage")))
        (assert-equal #:expect 0 #:got (car result))
        (assert-equal #:expect 2 #:got (publications (cdr result) buffer-uri)
          #:name "the buffer was checked against the new settings")
        (assert-true
          (and (find (lambda (message)
                       (string-contains (json-ref-path message "params" "message")
                                        "configuration"))
                     logs)
            #t)
          #:name "and the editor can see that it happened")))

    (test "the server says what it did in the editor's log"
      ;; A linter with nothing to say reads exactly like a linter that is not
      ;; running; the findings going out are worth a line in the log an editor
      ;; keeps for exactly this.
      (let* ((result (session (list (initialize-request 1) (open unclean))))
             (logs (notifications (cdr result) "window/logMessage"))
             (messages (map (lambda (message)
                              (json-ref-path message "params" "message"))
                            logs)))
        (assert-true (pair? logs) #:name "the server logged something")
        (assert-equal #:expect 3
          #:got (json-ref-path (car logs) "params" "type")
          #:name "as information, not as a complaint")
        (assert-true
          (and (find (lambda (message)
                       (and message (string-contains message "finding")))
                     messages)
            #t)
          #:name "and named the findings it published")))

    (test "a client with no language for the file still gets its findings"
      ;; `languageId' is what tells sift which Lisp it is reading, and a
      ;; client that has none for the file sends a null.  A server that raised
      ;; on it would have no document to answer about for the rest of the
      ;; session, which looks like a linter that does nothing.
      (let* ((result (session (list (initialize-request 1)
                                    (open-at buffer-uri unclean #:language json-null)
                                    (change unclean #:version 2))))
             (diagnostics (published (cdr result) buffer-uri)))
        (assert-equal #:expect 0 #:got (car result))
        (assert-true (and diagnostics (> (vector-length diagnostics) 0))
          #:name "the finding was published from the file's own extension")
        (assert-true (null? (notifications (cdr result) "window/showMessage"))
          #:name "and nothing was wrong enough to interrupt anybody")))

    (test "closing a buffer takes its findings away"
      (let* ((result (session (list (initialize-request 1)
                                    (open unclean) (close))))
             (diagnostics (published (cdr result) buffer-uri)))
        (assert-equal #:expect 0 #:got (vector-length diagnostics)
          #:name "the last word about the file is that it has no findings")))

    (test "a code action offers the rule's own fix"
      (let* ((result (session (list (initialize-request 1)
                                    (open unclean)
                                    (code-action-request 2 1 0))))
             (actions (json-ref (answer-to (cdr result) 2) "result"))
             (action (and actions (> (vector-length actions) 0)
                       (vector-ref actions 0)))
             (changes (json-ref (json-ref action "edit") "documentChanges"))
             (edits (json-ref (vector-ref changes 0) "edits"))
             (replacement (json-ref (vector-ref edits 0) "newText")))
        (assert-true (and action #t) #:name "an action was offered")
        (assert-equal #:expect "quickfix" #:got (json-ref action "kind"))
        (assert-contains #:expect (json-ref action "title") #:got "SIFT012")
        (assert-contains #:expect replacement #:got "when"
          #:name "the fix is the one that turns if into when")))

    (test "one action fixes every safe finding in the buffer"
      (let* ((result (session (list (initialize-request 1)
                                    (open unclean)
                                    (code-action-only-request 2 1 4
                                                              '("source.fixAll")))))
             (actions (json-ref (answer-to (cdr result) 2) "result"))
             (action (and (> (vector-length actions) 0) (vector-ref actions 0)))
             (changes (json-ref (json-ref action "edit") "documentChanges"))
             (edits (json-ref (vector-ref changes 0) "edits"))
             (replacement (json-ref (vector-ref edits 0) "newText")))
        (assert-equal #:expect 1 #:got (vector-length actions)
          #:name "one action, not one per finding")
        (assert-equal #:expect "source.fixAll.sift" #:got (json-ref action "kind"))
        (assert-contains #:expect replacement #:got "when"
          #:name "the `if' was rewritten")
        (assert-equal #:expect #f #:got (string-contains replacement "   \n")
          #:name "and the trailing whitespace went with it")))

    (test "a client that asks for quickfixes gets quickfixes and nothing else"
      (let* ((result (session (list (initialize-request 1)
                                    (open unclean)
                                    (code-action-only-request 2 1 4 '("quickfix")))))
             (actions (vector->list
                       (json-ref (answer-to (cdr result) 2) "result")))
             (kinds (delete-duplicates
                     (map (lambda (action) (json-ref action "kind")) actions))))
        (assert-true (pair? kinds) #:name "something was offered")
        (assert-equal #:expect '("quickfix") #:got kinds
          #:name "no rewrite and no fix-all were mixed in")))

    (test "the definition under the cursor can be reformatted on its own"
      (let* ((text "(define (f x)\n    (* x x))\n\n(define (g) 1)\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (code-action-only-request 2 1 6
                                                              '("refactor.rewrite")))))
             (actions (json-ref (answer-to (cdr result) 2) "result"))
             (action (vector-ref actions 0))
             (changes (json-ref (json-ref action "edit") "documentChanges"))
             (edits (json-ref (vector-ref changes 0) "edits"))
             (edit (vector-ref edits 0))
             (replacement (json-ref edit "newText")))
        (assert-equal #:expect "refactor.rewrite" #:got (json-ref action "kind"))
        (assert-contains #:expect replacement #:got "  (* x x))"
          #:name "the body was indented")
        (assert-equal #:expect 0
          #:got (json-ref (json-ref (json-ref edit "range") "start") "line")
          #:name "the edit starts at the definition")
        (assert-equal #:expect 1
          #:got (json-ref (json-ref (json-ref edit "range") "end") "line")
          #:name "and ends with it, leaving the next definition alone")))

    (test "organizing imports puts the specs in order"
      (let* ((text "(import (srfi 1) (scheme base))\n(display 1)\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (code-action-only-request 2 1 0
                                                              '("source.organizeImports")))))
             (actions (json-ref (answer-to (cdr result) 2) "result"))
             (action (and (> (vector-length actions) 0) (vector-ref actions 0)))
             (changes (json-ref (json-ref action "edit") "documentChanges"))
             (edits (json-ref (vector-ref changes 0) "edits"))
             (replacement (json-ref (vector-ref edits 0) "newText")))
        (assert-equal #:expect "source.organizeImports" #:got (json-ref action "kind"))
        (assert-same-text #:expect "(import (scheme base) (srfi 1))\n(display 1)\n"
                          #:got replacement
                          #:name "the library that sorts first is written first")))

    (test "organizing imports sorts the use-module clauses of a define-module"
      (let* ((text (string-append
                    "(define-module (app util)\n"
                    "  #:use-module (srfi srfi-1)\n"
                    "  #:use-module (ice-9 format)\n"
                    "  #:export (f))\n"))
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (code-action-only-request 2 1 0
                                                              '("source.organizeImports")))))
             (actions (json-ref (answer-to (cdr result) 2) "result"))
             (action (and (> (vector-length actions) 0) (vector-ref actions 0)))
             (changes (json-ref (json-ref action "edit") "documentChanges"))
             (edits (json-ref (vector-ref changes 0) "edits"))
             (replacement (json-ref (vector-ref edits 0) "newText")))
        (assert-same-text
         #:expect (string-append
                   "(define-module (app util)\n"
                   "  #:use-module (ice-9 format)\n"
                   "  #:use-module (srfi srfi-1)\n"
                   "  #:export (f))\n")
         #:got replacement
         #:name "the clause whose library sorts first is written first")))

    (test "organizing imports removes the ones nothing uses"
      (let* ((text "(import (scheme base) (only (srfi 1) car))\n(display 1)\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (code-action-only-request 2 1 0
                                                              '("source.organizeImports")))))
             (actions (json-ref (answer-to (cdr result) 2) "result"))
             (action (and (> (vector-length actions) 0) (vector-ref actions 0)))
             (changes (json-ref (json-ref action "edit") "documentChanges"))
             (edits (json-ref (vector-ref changes 0) "edits"))
             (replacement (json-ref (vector-ref edits 0) "newText")))
        (assert-same-text #:expect "(import (scheme base))\n(display 1)\n"
                          #:got replacement
                          #:name "the unused import went, and the file still reads")))

    (test "optimize imports and sort imports are two actions by default"
      (let* ((text "(import (only (srfi 1) car) (scheme base))\n(display 1)\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (code-action-request 2 1 0))))
             (actions (vector->list
                       (json-ref (answer-to (cdr result) 2) "result")))
             (titles (map (lambda (action) (json-ref action "title")) actions)))
        (assert-true (and (member "sift: sort imports" titles) #t)
          #:name "the sorting action is offered without being asked for")
        (assert-true (and (member "sift: optimize imports" titles) #t)
          #:name "and so is the one that removes unused imports")))

    (test "sorting imports can be turned off and optimizing stays on"
      (let* ((text "(import (only (srfi 1) car) (scheme base))\n(display 1)\n")
             (result (session (list (initialize-request 1)
                                    (configuration-with
                                     (settings-object
                                      (list (cons "sort-imports" json-false))))
                                    (open text)
                                    (code-action-request 2 1 0))))
             (actions (vector->list
                       (json-ref (answer-to (cdr result) 2) "result")))
             (titles (map (lambda (action) (json-ref action "title")) actions)))
        (assert-true (not (member "sift: sort imports" titles))
          #:name "the sorting action is gone")
        (assert-true (and (member "sift: optimize imports" titles) #t)
          #:name "the other one stays")))

    (test "an import that is already organized has no action"
      (let* ((text "(import (scheme base) (srfi 1))\n(display 1)\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (code-action-only-request 2 1 0
                                                              '("source.organizeImports")))))
             (actions (json-ref (answer-to (cdr result) 2) "result")))
        (assert-equal #:expect 0 #:got (vector-length actions)
          #:name "nothing to offer, and nothing offered")))

    (test "an expression can be extracted to a let binding"
      (let* ((text "(define (area w h)\n  (+ (* w h) 1))\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (code-action-only-request 2 1 6
                                                              '("refactor.extract")))))
             (actions (vector->list
                       (json-ref (answer-to (cdr result) 2) "result")))
             (action (find (lambda (candidate)
                             (string-contains (json-ref candidate "title") "bind"))
                           actions))
             (changes (and action (json-ref (json-ref action "edit") "documentChanges")))
             (edits (and changes (json-ref (vector-ref changes 0) "edits")))
             (replacement (and edits (json-ref (vector-ref edits 0) "newText"))))
        (assert-true (and action #t) #:name "an extraction was offered")
        (assert-equal #:expect "refactor.extract" #:got (json-ref action "kind"))
        (assert-same-text #:expect "(let ((value (* w h))) (+ value 1))"
                          #:got replacement
                          #:name "the expression was hoisted into a binding")))

    (test "an expression can be extracted to a procedure"
      (let* ((text "(define (area w h)\n  (+ (* w h) 1))\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (code-action-only-request 2 1 6
                                                              '("refactor.extract")))))
             (actions (vector->list
                       (json-ref (answer-to (cdr result) 2) "result")))
             (action (find (lambda (candidate)
                             (string-contains (json-ref candidate "title") "procedure"))
                           actions))
             (changes (and action (json-ref (json-ref action "edit") "documentChanges")))
             (edits (and changes (vector->list (json-ref (vector-ref changes 0) "edits"))))
             (texts (if edits (map (lambda (edit) (json-ref edit "newText")) edits) '())))
        (assert-equal #:expect "refactor.extract" #:got (json-ref action "kind"))
        (assert-equal #:expect "(extracted w h)" #:got (car texts)
          #:name "the expression is replaced by the call")
        (assert-contains #:expect (cadr texts) #:got "(define (extracted w h)"
          #:name "and the procedure takes the enclosing parameters")))

    (test "an expression in a let binding is extracted around the let"
      (let* ((text "(let ((x (compute 1)))\n  x)\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (code-action-only-request 2 0 11
                                                              '("refactor.extract")))))
             (actions (vector->list
                       (json-ref (answer-to (cdr result) 2) "result")))
             (action (find (lambda (candidate)
                             (string-contains (json-ref candidate "title") "bind"))
                           actions))
             (changes (and action (json-ref (json-ref action "edit") "documentChanges")))
             (edits (and changes (json-ref (vector-ref changes 0) "edits")))
             (replacement (and edits (json-ref (vector-ref edits 0) "newText"))))
        (assert-true (and action #t) #:name "the binding value can be extracted")
        (assert-contains #:expect replacement
          #:got "(let ((compute-2 (compute 1))) (let ((x compute-2))"
          #:name "the let is wrapped, not the binding list")))

    (test "a lambda that is a definition's value keeps the definition's name"
      (let* ((text "(define square (lambda (x) (* x x)))\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (code-action-only-request 2 0 17
                                                              '("refactor.extract")))))
             (actions (vector->list
                       (json-ref (answer-to (cdr result) 2) "result")))
             (action (find (lambda (candidate)
                             (string-contains (json-ref candidate "title")
                                              "write this lambda"))
                           actions))
             (changes (and action (json-ref (json-ref action "edit") "documentChanges")))
             (edits (and changes (json-ref (vector-ref changes 0) "edits")))
             (replacement (and edits (json-ref (vector-ref edits 0) "newText"))))
        (assert-true (and action #t) #:name "the definition shape was offered")
        (assert-equal #:expect "(define (square x)\n  (* x x))"
          #:got replacement
          #:name "named after the definition, not `procedure`")))

    (test "a client that asks for quickfixes gets no extraction"
      (let* ((text "(define (area w h)\n  (+ (* w h) 1))\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (code-action-only-request 2 1 6 '("quickfix")))))
             (actions (vector->list
                       (json-ref (answer-to (cdr result) 2) "result")))
             (kinds (delete-duplicates
                     (map (lambda (action) (json-ref action "kind")) actions))))
        (assert-equal #:expect #f #:got (member "refactor.extract" kinds)
          #:name "no extraction was mixed in")))

    (test "a lambda can be extracted to a procedure of its own"
      (let* ((text "(define (squares xs)\n  (map (lambda (x) (* x x)) xs))\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (code-action-only-request 2 1 8
                                                              '("refactor.extract")))))
             (actions (vector->list
                       (json-ref (answer-to (cdr result) 2) "result")))
             (action (find (lambda (candidate)
                             (string-contains (json-ref candidate "title") "lambda"))
                           actions))
             (changes (and action (json-ref (json-ref action "edit") "documentChanges")))
             (edits (and changes (vector->list (json-ref (vector-ref changes 0) "edits"))))
             (texts (if edits (map (lambda (edit) (json-ref edit "newText")) edits) '())))
        (assert-true (and action #t) #:name "the lambda was offered as a procedure")
        (assert-equal #:expect "procedure" #:got (car texts)
          #:name "the name takes the lambda's place")
        (assert-contains #:expect (cadr texts) #:got "(define (procedure x)"
          #:name "and the definition keeps the lambda's own formals")
        (assert-contains #:expect (cadr texts) #:got "(* x x)"
          #:name "and its body, as it was written")))

    (test "a call to a single-expression procedure can be inlined"
      (let* ((text "(define (square x)\n  (* x x))\n\n(display (square 3))\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (code-action-only-request 2 3 14
                                                              '("refactor.inline")))))
             (actions (vector->list
                       (json-ref (answer-to (cdr result) 2) "result")))
             (action (find (lambda (candidate)
                             (string-contains (json-ref candidate "title") "inline"))
                           actions))
             (changes (and action (json-ref (json-ref action "edit") "documentChanges")))
             (edits (and changes (json-ref (vector-ref changes 0) "edits")))
             (replacement (and edits (json-ref (vector-ref edits 0) "newText"))))
        (assert-equal #:expect "refactor.inline" #:got (json-ref action "kind"))
        (assert-same-text #:expect "(* 3 3)" #:got replacement
                          #:name "the argument took the parameter's place")))

    (test "a call sift cannot inline is left alone"
      ;; The body rebinds the parameter, so the argument would land inside a
      ;; binding rather than where the parameter was read.
      (let* ((text "(define (twice x)\n  (let ((x (f x))) (* x 2)))\n\n(twice 3)\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (code-action-only-request 2 3 5
                                                              '("refactor.inline")))))
             (actions (json-ref (answer-to (cdr result) 2) "result")))
        (assert-equal #:expect 0 #:got (vector-length actions)
          #:name "refused rather than approximated")))

    (test "signature help answers for a procedure that takes nothing"
      (let* ((text "(define (now)\n  (current-time))\n\n(now )\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (signature-request 10 3 5))))
             (answer (json-ref (answer-to (cdr result) 10) "result"))
             (signature (and answer
                          (vector-ref (json-ref answer "signatures") 0))))
        (assert-equal #:expect "(now)" #:got (json-ref signature "label")
          #:name "one name, and no arguments")))

    (test "formatting answers with the text sift would write"
      (let* ((result (session (list (initialize-request 1)
                                    (open unclean)
                                    (formatting-request 3))))
             (edits (json-ref (answer-to (cdr result) 3) "result"))
             (edit (and edits (> (vector-length edits) 0)
                     (vector-ref edits 0))))
        (assert-true (and edit #t) #:name "one edit came back")
        (assert-contains #:expect (json-ref edit "newText")
          #:got "  (if (ready?)"
          #:name "the edit is the buffer, indented")))

    (test "the hover says what the rule wants"
      (let* ((result (session (list (initialize-request 1)
                                    (open unclean)
                                    (hover-request 4 1 4))))
             (message (json-ref (answer-to (cdr result) 4) "result"))
             (value (json-ref (json-ref message "contents") "value")))
        (assert-contains #:expect value #:got "SIFT012")
        (assert-contains #:expect value #:got "if-without-else")))

    (test "signature help answers with the definition's own formal list"
      (let* ((text "(define (greet name)\n  \"Greet NAME.\"\n  (message \"hi\" name))\n\n(greet )\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (signature-request 7 4 7))))
             (answer (json-ref (answer-to (cdr result) 7) "result"))
             (signature (and answer
                          (vector-ref (json-ref answer "signatures") 0))))
        (assert-equal #:expect "(greet name)" #:got (json-ref signature "label"))
        (assert-equal #:expect 0 #:got (json-ref answer "activeParameter")
          #:name "the cursor is in the first argument")
        (assert-equal #:expect "Greet NAME."
          #:got (json-ref (json-ref signature "documentation") "value")
          #:name "the docstring comes with it")))

    (test "signature help counts the arguments already written"
      (let* ((text "(define (greet name punctuation)\n  \"Greet NAME.\"\n  name)\n\n(greet \"x\" )\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (signature-request 8 4 11))))
             (answer (json-ref (answer-to (cdr result) 8) "result")))
        (assert-equal #:expect 1 #:got (json-ref answer "activeParameter")
          #:name "one argument is finished, so the second is being written")))

    (test "signature help says nothing about a call sift cannot see"
      (let* ((text "(mystery 1)\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (signature-request 9 0 6))))
             (answer (json-ref (answer-to (cdr result) 9) "result")))
        (assert-equal #:expect 'null #:got answer
          #:name "no invented signature")))

    (test "inlay hints name the positional arguments of a call"
      (let* ((text "(define (make-window parent width height title)\n  (list parent width height title))\n\n(make-window root 640 480 \"Main\")\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (inlay-hint-request 11 3))))
             (hints (json-ref (answer-to (cdr result) 11) "result"))
             (labels (map (lambda (hint) (json-ref hint "label"))
                          (vector->list hints))))
        (assert-equal #:expect 0 #:got (car result))
        ;; the label carries one space on each side, so it reads as a word
        ;; beside the argument whether or not the client honours the padding
        ;; flags -- and never as two spaces, which is a hint pushed away from
        ;; the argument it names
        (assert-equal #:expect '(" parent " " width " " height " " title ")
          #:got labels
          #:name "one hint per argument, named by the definition")
        (assert-equal #:expect 2
          #:got (json-ref (vector-ref hints 0) "kind")
          #:name "and each one is a parameter, which is how an editor styles it")))

    (test "an inlay hint sits where its argument starts"
      (let* ((text "(define (greet name)\n  name)\n\n(greet world)\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (inlay-hint-request 12 3))))
             (hints (json-ref (answer-to (cdr result) 12) "result"))
             (hint (and (> (vector-length hints) 0) (vector-ref hints 0))))
        (assert-equal #:expect 3
          #:got (json-ref (json-ref hint "position") "line")
          #:name "on the line of the call, counted from zero")
        (assert-equal #:expect " name " #:got (json-ref hint "label"))))

    (test "a keyword argument is not hinted"
      ;; `(greet/who "x" #:loud? #t)' says its own names from the first
      ;; keyword on, and a hint over `#t' would be reading the call as if the
      ;; keyword were not there.
      (let* ((text "(define* (greet/who name #:key loud?)\n  name)\n\n(greet/who \"x\" #:loud? #t)\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (inlay-hint-request 13 3))))
             (hints (json-ref (answer-to (cdr result) 13) "result"))
             (labels (map (lambda (hint) (json-ref hint "label"))
                          (vector->list hints))))
        (assert-equal #:expect '(" name ") #:got labels
          #:name "only the positional argument is named")))

    (test "a literal's type is hinted when the project asks for it"
      ;; A type hint is the one hint that could look like inference, so it is
      ;; off unless asked for and it only ever repeats what the source says:
      ;; `"Main"' is a string because it is written with quotes.
      (let* ((text "(define (make-window parent width title)\n  (list parent width title))\n\n(make-window root 640 \"Main\")\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (configuration-with
                                     (json-object
                                      (field "sift"
                                             (json-object
                                              (field "inlay-hints"
                                                     (list "parameters" "types"))))))
                                    (inlay-hint-request 21 3))))
             (hints (vector->list (json-ref (answer-to (cdr result) 21) "result")))
             (named (lambda (kind)
                      (map (lambda (hint) (json-ref hint "label"))
                           (filter (lambda (hint)
                                     (equal? kind (json-ref hint "kind")))
                                   hints)))))
        (assert-equal #:expect '(" parent " " width " " title ") #:got (named 2)
          #:name "the parameters are named, as before")
        (assert-equal #:expect '(" number " " string ") #:got (named 1)
          #:name "and a literal says its own type")))

    (test "a name says nothing about its type"
      ;; `root` is a variable: sift would have to follow it to say what it
      ;; holds, and a hint that guessed would be worse than a blank space.
      (let* ((text "(define (make-window parent width)\n  (list parent width))\n\n(make-window root 640)\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (configuration-with
                                     (json-object
                                      (field "sift"
                                             (json-object
                                              (field "inlay-hints"
                                                     (list "types"))))))
                                    (inlay-hint-request 22 3))))
             (hints (json-ref (answer-to (cdr result) 22) "result"))
             (labels (map (lambda (hint) (json-ref hint "label"))
                          (vector->list hints))))
        (assert-equal #:expect '(" number ") #:got labels
          #:name "only the argument whose type is written down")))

    (test "the project chooses which kinds of hint it gets"
      (let* ((text "(define (make-window parent width)\n  (list parent width))\n\n(make-window root 640)\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (configuration-with
                                     (json-object
                                      (field "sift"
                                             (json-object
                                              (field "inlay-hints" (list "types"))))))
                                    (inlay-hint-request 23 3))))
             (hints (json-ref (answer-to (cdr result) 23) "result"))
             (kinds (delete-duplicates
                     (map (lambda (hint) (json-ref hint "kind"))
                          (vector->list hints)))))
        (assert-equal #:expect '(1) #:got kinds
          #:name "types alone means no parameter names")))

    (test "a call sift cannot resolve gets no inlay hint"
      (let* ((text "(mystery 1 2)\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (inlay-hint-request 14 0))))
             (hints (json-ref (answer-to (cdr result) 14) "result")))
        (assert-equal #:expect 0 #:got (vector-length hints)
          #:name "an invented name would be worse than none")))

    (test "a parameter list is not a call to the procedure it defines"
      ;; `(define (f a b) ...)' has `a' and `b' in argument position that
      ;; nothing is passed to: a hint there would describe a definition as if
      ;; it were a call to itself.
      (let* ((text "(define (f alpha beta)\n  alpha)\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (inlay-hint-request 15 0))))
             (hints (json-ref (answer-to (cdr result) 15) "result")))
        (assert-equal #:expect 0 #:got (vector-length hints))))

    (test "the editor's settings are part of the configuration"
      ;; `eglot-workspace-configuration' is a JSON object the client sends
      ;; when the session starts and again whenever it changes; sift reads it
      ;; as the layer above its own files.
      (let* ((text "(define (make-window parent width)\n  parent)\n\n(make-window root 640)\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (configuration-with
                                     (json-object
                                      (field "sift"
                                             (json-object
                                              (field "inlay-hints" 'false)))))
                                    (inlay-hint-request 16 3))))
             (hints (json-ref (answer-to (cdr result) 16) "result")))
        (assert-equal #:expect 0 #:got (car result)
          #:name "a setting the server does not have a file for is not a failure")
        (assert-equal #:expect 0 #:got (vector-length hints)
          #:name "and turning the hints off turns them off")))

    (test "a setting the client sends with initialize is the configuration too"
      (let* ((text "(define (make-window parent width)\n  parent)\n\n(make-window root 640)\n")
             (result (session (list (request 17 "initialize"
                                             (json-object
                                              (field "capabilities" (json-object))
                                              (field "initializationOptions"
                                                     (json-object
                                                      (field "sift"
                                                             (json-object
                                                              (field "inlay-hints" 'false)))))))
                                    (open-with-text text)
                                    (inlay-hint-request 18 3))))
             (hints (json-ref (answer-to (cdr result) 18) "result")))
        (assert-equal #:expect 0 #:got (vector-length hints)
          #:name "the session started with the editor's settings")))

    (test "a setting the client sends that sift does not have is reported"
      (let* ((text "(define (f) 1)\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (configuration-with
                                     (json-object
                                      (field "sift"
                                             (json-object
                                              (field "inlay-hint" 'false))))))))
             (logs (map (lambda (message)
                          (json-ref-path message "params" "message"))
                        (notifications (cdr result) "window/logMessage"))))
        (assert-true
          (and (find (lambda (message)
                       (and message (string-contains message "inlay-hint")))
                     logs)
            #t)
          #:name "the editor can see that the setting went nowhere")))

    (test "completion puts the name being typed first"
      (let* ((text "(define (alpha-one) 1)\n(define (beta) 2)\n(al\n")
             (result (session (list (initialize-request 1)
                                    (open-with-text text)
                                    (completion-request 10 2 2))))
             (items (json-ref (answer-to (cdr result) 10) "result"))
             (first (and items (> (vector-length items) 0) (vector-ref items 0))))
        (assert-equal #:expect "alpha-one" #:got (json-ref first "label")
          #:name "the prefix match is offered first")
        (assert-contains #:expect (json-ref first "sortText" "") #:got "alpha-one"
          #:name "and its sort text says why")))

    (test "an unknown request is refused, and the server keeps talking"
      (let* ((result (session (list (initialize-request 1)
                                    (open unclean)
                                    (request 5 "textDocument/typeHierarchy"
                                             (json-object))
                                    (formatting-request 6))))
             (refusal (answer-to (cdr result) 5))
             (later (answer-to (cdr result) 6)))
        (assert-equal #:expect -32601
          #:got (json-ref (json-ref refusal "error") "code")
          #:name "the method is not implemented")
        (assert-true (and later #t) #:name "the next request was still answered")))

    (test "shutdown is answered and exit ends the session"
      (let ((result (session (list (initialize-request 1)
                                   (open unclean)
                                   (shutdown-request 7)
                                   (exit-message)))))
        (assert-equal #:expect 0 #:got (car result))
        (assert-equal #:expect 'null
          #:got (json-ref (answer-to (cdr result) 7) "result"))))

    (test "the dialect follows the language identifier the editor sent"
      (let* ((text "(defun f (x)\n  (message \"%s\" x))\n")
             (result (session (list (initialize-request 1)
                                    (open text #:language "emacs-lisp"))))
             (diagnostic (diagnostic-with (cdr result) "SIFT017")))
        (assert-true (and diagnostic #t)
          #:name "the rule that only Emacs Lisp has fired")))

    (test "a buffer that does not parse has findings, not silence"
      (let* ((result (session (list (initialize-request 1)
                                    (open unbalanced))))
             (diagnostics (published (cdr result) buffer-uri))
             (errors (and diagnostics
                       (filter (lambda (diagnostic)
                                 (equal? 1 (diagnostic-severity diagnostic)))
                               (vector->list diagnostics)))))
        (assert-equal #:expect 0 #:got (car result))
        (assert-true (and errors (pair? errors))
          #:name "the file that never closes is reported as an error")
        (assert-true (and errors
                       (let ((code (diagnostic-code (car errors))))
                         (and code (not (string-null? code)))))
          #:name "and the finding is named, not anonymous")))

    (test "the dialect layer still answers about a buffer sift cannot finish"
      ;; A half-written form is the normal state of a buffer being typed: the
      ;; findings for the part that could be read, and the actions that apply
      ;; to them, must not go away because the end of the file is missing.
      (let* ((result (session (list (initialize-request 1)
                                    (open unbalanced)
                                    (code-action-request 2 0 0))))
             (actions (json-ref (answer-to (cdr result) 2) "result")))
        (assert-equal #:expect 0 #:got (car result))
        (assert-true (vector? actions) #:name "a code action array came back")))

    (test "formatting a buffer that does not parse says why instead of nothing"
      (let ((result (session (list (initialize-request 1)
                                   (open unbalanced)
                                   (formatting-request 2)))))
        (assert-equal #:expect 0 #:got (car result))
        (assert-equal #:expect 0
          #:got (vector-length (json-ref (answer-to (cdr result) 2) "result"))
          #:name "no edit is offered, because sift does not guess")
        (let ((shown (notifications (cdr result) "window/showMessage")))
          (assert-true (and shown (pair? shown))
            #:name "the editor is told that formatting was refused")
          (when (pair? shown)
            (assert-equal #:expect 1
              #:got (json-ref-path (car shown) "params" "type")
              #:name "as an error")
            (assert-contains #:expect
              (json-ref-path (car shown) "params" "message")
              #:got "cannot format")))))

    (test "a message that is not JSON is answered, and the session goes on"
      (let* ((result (session (list (initialize-request 1)
                                    (raw "{this is not json}")
                                    (formatting-request 2))))
             (refusal (answer-to (cdr result) 1))
             (answer (answer-to (cdr result) 2)))
        (assert-equal #:expect 0 #:got (car result)
          #:name "the server exited cleanly")
        (assert-true (and refusal #t) #:name "initialize was answered")
        (assert-true (and answer #t)
          #:name "the request after the bad message was answered too")))

    (test "a request before initialize is refused by the protocol's rule"
      (let* ((result (session (list (formatting-request 1))))
             (answer (answer-to (cdr result) 1)))
        (assert-equal #:expect -32002
          #:got (json-ref (json-ref answer "error") "code")
          #:name "the server says it has not been initialized")))

    (test "an unknown method is refused and the server keeps its answers"
      (let* ((result (session (list (initialize-request 1)
                                    (request 2 "textDocument/typeDefinition" (json-object))
                                    (shutdown-request 3)
                                    (exit-message))))
             (refusal (answer-to (cdr result) 2)))
        (assert-equal #:expect -32601
          #:got (json-ref (json-ref refusal "error") "code"))
        (assert-equal #:expect 0 #:got (car result)
          #:name "shutdown and exit end the server with 0")))

    (test "exit without a shutdown ends with the status the protocol asks for"
      (let ((result (session (list (initialize-request 1) (exit-message)))))
        (assert-equal #:expect 1 #:got (car result))))

    (test "a change sent as a range is applied to the text"
      ;; sift asks for full synchronisation, but a client that sends a range
      ;; anyway is a client to work with: the range is spliced into the buffer
      ;; sift already has.
      (let* ((result (session (list (initialize-request 1)
                                    (open unclean)
                                    (json-object
                                     (field "jsonrpc" "2.0")
                                     (field "method" "textDocument/didChange")
                                     (field "params"
                                            (json-object
                                             (field "textDocument"
                                                    (json-object (field "uri" buffer-uri)
                                                                 (field "version" 2)))
                                             (field "contentChanges"
                                                    (vector
                                                     (json-object
                                                      (field "range"
                                                             (range (point 1 2) (point 1 4)))
                                                      (field "text" "unless"))))))))))
             (texts (filter (lambda (diagnostic)
                              (equal? "SIFT012" (diagnostic-code diagnostic)))
                            (vector->list (published (cdr result) buffer-uri)))))
        (assert-equal #:expect 0 #:got (car result))
        (assert-equal #:expect '()
          #:got (map diagnostic-code texts)
          #:name "the `if` the range replaced is gone")))

    (test "a notification that raises does not take the server with it"
      (let* ((result (session (list (initialize-request 1)
                                    (open-with-text 42)
                                    (shutdown-request 2)
                                    (exit-message)))))
        (assert-true (pair? (notifications (cdr result) "window/showMessage"))
          #:name "the editor was told the request was not usable")
        (assert-equal #:expect 'null
          #:got (json-ref (answer-to (cdr result) 2) "result")
          #:name "and the next request was answered normally")))

    (test "a configuration the server was started with is what it answers with"
      (let* ((result (session (list (initialize-request 1) (open unclean))
                              #:arguments '("--severity=error")))
             (diagnostics (published (cdr result) buffer-uri)))
        (assert-equal #:expect 0 #:got (vector-length diagnostics)
          #:name "a warning is below the severity the server was given")))

    ;; --- what the editor can navigate to --------------------------------

    (test "initialize advertises the navigation requests"
      (let* ((result (session (list (initialize-request 1))))
             (capabilities (json-ref (json-ref (answer-to (cdr result) 1) "result")
                                     "capabilities")))
        (assert-equal #:expect 'true #:got (json-ref capabilities "definitionProvider")
          #:name "definitions")
        (assert-equal #:expect 'true #:got (json-ref capabilities "referencesProvider")
          #:name "references")
        (assert-equal #:expect 'true #:got (json-ref capabilities "documentHighlightProvider")
          #:name "highlights")
        (assert-equal #:expect 'true #:got (json-ref capabilities "documentSymbolProvider")
          #:name "an outline")
        (assert-equal #:expect 'true #:got (json-ref capabilities "workspaceSymbolProvider")
          #:name "a workspace search")
        (assert-equal #:expect 'true
          #:got (json-ref (json-ref capabilities "renameProvider") "prepareProvider")
          #:name "a rename, with a pre-flight check")))

    (test "a symbol jumps to the definition that names it"
      (let* ((text "(define (square x)\n  (* x x))\n\n(define (area r)\n  (square r))\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (definition-request 2 buffer-uri 4 4))))
             (locations (json-ref (answer-to (cdr result) 2) "result"))
             (start (start-of (vector-ref locations 0))))
        (assert-equal #:expect 1 #:got (vector-length locations)
          #:name "one definition, not every `square' in the file")
        (assert-equal #:expect 0 #:got (json-ref start "line"))
        (assert-equal #:expect 9 #:got (json-ref start "character"))))

    (test "a reference inside a let jumps to the binding, not the outer name"
      ;; Both `x's are named `x'; only one of them is the one the reference
      ;; means, and the answer that ignores scope is wrong in every file that
      ;; uses a name twice.
      (let* ((text "(define (f x)\n  (let ((x 1))\n    x))\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (definition-request 2 buffer-uri 2 4))))
             (locations (json-ref (answer-to (cdr result) 2) "result"))
             (start (start-of (vector-ref locations 0))))
        (assert-equal #:expect 1 #:got (json-ref start "line")
          #:name "the `let' binding, on the second line")
        (assert-equal #:expect 9 #:got (json-ref start "character"))))

    (test "the name of a parameter is its own definition"
      (let* ((text "(define (f x)\n  x)\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (definition-request 2 buffer-uri 0 11))))
             (locations (json-ref (answer-to (cdr result) 2) "result"))
             (start (start-of (vector-ref locations 0))))
        (assert-equal #:expect 1 #:got (vector-length locations)
          #:name "a request on the binding answers with the binding")
        (assert-equal #:expect 11 #:got (json-ref start "character"))))

    (test "an Emacs Lisp defun is a definition"
      (let* ((text "(defun greet (name)\n  (greet name))\n")
             (result (session (list (initialize-request 1)
                                    (open-at buffer-uri text #:language "emacs-lisp")
                                    (definition-request 2 buffer-uri 1 3))))
             (locations (json-ref (answer-to (cdr result) 2) "result"))
             (start (start-of (vector-ref locations 0))))
        (assert-equal #:expect 0 #:got (json-ref start "line"))
        (assert-equal #:expect 7 #:got (json-ref start "character")
          #:name "`greet' behind the `defun'")))

    (test "a Common Lisp defparameter is a definition"
      (let* ((text "(defparameter *limit* 10)\n\n(defun clamp (x)\n  (min *limit* x))\n")
             (result (session (list (initialize-request 1)
                                    (open-at buffer-uri text #:language "lisp")
                                    (definition-request 2 buffer-uri 3 9))))
             (locations (json-ref (answer-to (cdr result) 2) "result"))
             (start (start-of (vector-ref locations 0))))
        (assert-equal #:expect 0 #:got (json-ref start "line"))
        (assert-equal #:expect 14 #:got (json-ref start "character"))))

    (test "a symbol with no definition answers with nothing at all"
      (let* ((result (session (list (initialize-request 1)
                                    (open "(define (f) 1)\n(f)\n")
                                    (definition-request 2 buffer-uri 1 1))))
             (locations (json-ref (answer-to (cdr result) 2) "result")))
        (assert-equal #:expect 1 #:got (vector-length locations)
          #:name "`f' is defined, so this is the definition")
        (let* ((unknown (session (list (initialize-request 1)
                                       (open "(define (f) 1)\n(nowhere 2)\n")
                                       (definition-request 2 buffer-uri 1 2))))
               (none (json-ref (answer-to (cdr unknown) 2) "result")))
          (assert-equal #:expect 0 #:got (vector-length none)
            #:name "a name nothing defines is an empty answer, not an error"))))

    (test "a definition in another buffer is found"
      (let* ((result (session (list (initialize-request 1)
                                    (open-at lib-uri lib-source)
                                    (open "(define (area r)\n  (square r))\n")
                                    (definition-request 2 buffer-uri 1 4))))
             (locations (json-ref (answer-to (cdr result) 2) "result"))
             (start (start-of (vector-ref locations 0))))
        (assert-equal #:expect 1 #:got (vector-length locations))
        (assert-equal #:expect lib-uri
          #:got (json-ref (vector-ref locations 0) "uri")
          #:name "the jump leaves the buffer it started in")
        (assert-equal #:expect 9 #:got (json-ref start "character"))))

    (test "a jump is answered from the project when the client names a root"
      ;; A root is what turns "the project" from this session's buffers into
      ;; the files on disk, and it is reached through an index kept in a cache
      ;; file named by a hash of that root.  A test without a root never walks
      ;; that path, and neither does anything else in a session that has none
      ;; -- which is how a hash function naming a variable Guile does not have
      ;; (`bitwise-xor' for `logxor') could leave every jump in a real editor
      ;; answering "internal error" while the suite was green.
      (with-temp-directory
       (lambda (directory)
         (with-cache-home
          directory
          (lambda ()
            (spit (string-append directory "/other.scm")
                  "(define (neighbour x)\n  (* x x))\n")
            (let* ((root (string-append "file://" directory))
                   (here (string-append root "/here.scm"))
                   (result (session-when-ready (list (initialize-with-root 1 root)
                                                     (open-at here "(neighbour 2)\n")
                                                     (definition-request 2 here 0 2))))
                   (locations (json-ref (answer-to (cdr result) 2) "result"))
                   (answer (and (vector? locations)
                             (> (vector-length locations) 0)
                             (vector-ref locations 0))))
              (assert-equal #:expect 0 #:got (car result))
              (assert-true (and answer #t)
                #:name "the file that defines the name was found")
              (when answer
                (assert-equal #:expect (string-append root "/other.scm")
                  #:got (json-ref answer "uri")
                  #:name "and it is the file on disk, not a buffer")
                (assert-equal #:expect 0
                  #:got (json-ref (start-of answer) "line")))))))))

    (test "references list every mention in the project"
      (let* ((result (session (list (initialize-request 1)
                                    (open-at lib-uri lib-source)
                                    (open "(define (area r)\n  (square r))\n")
                                    (references-request 2 buffer-uri 1 4))))
             (locations (json-ref (answer-to (cdr result) 2) "result"))
             (uris (map (lambda (location) (json-ref location "uri"))
                        (vector->list locations))))
        (assert-equal #:expect 2 #:got (vector-length locations)
          #:name "the definition and the use")
        (assert-equal #:expect (list buffer-uri lib-uri) #:got uris
          #:name "this buffer's mention first, then the other one's")))

    (test "references of a binding are the mentions inside it, not the project's"
      ;; `(let ((x 1)) ...)' makes an `x' that has nothing to do with the `x'
      ;; of the next definition, and a server that answered with every `x' in
      ;; the project would be answering a question nobody asked.
      (let* ((result (session (list (initialize-request 1)
                                    (open (string-append
                                           "(define (f y)\n"
                                           "  (let ((x 1))\n"
                                           "    (list x x)))\n"
                                           "(define (g x)\n"
                                           "  x)\n"))
                                    (references-request 2 buffer-uri 2 10))))
             (locations (json-ref (answer-to (cdr result) 2) "result")))
        (assert-equal #:expect 3 #:got (vector-length locations)
          #:name "the binding and the two mentions it has")))

    (test "references of a parameter are the uses in the definition"
      ;; The parameter is written in the argument list and used in the body;
      ;; the scope is the whole definition, not the list it is spelled in.
      (let* ((result (session (list (initialize-request 1)
                                    (open (string-append
                                           "(define (f x)\n"
                                           "  (list x x))\n"
                                           "(define (g x) x)\n"))
                                    (references-request 2 buffer-uri 0 11))))
             (locations (json-ref (answer-to (cdr result) 2) "result")))
        (assert-equal #:expect 3 #:got (vector-length locations)
          #:name "the parameter and the two mentions it has")))

    (test "a jump is answered from a compressed file in the project"
      ;; Emacs ships its library compressed; a project that vendors one has
      ;; the same shape, and a jump that cannot read it has nothing to land on.
      (with-temp-directory
       (lambda (directory)
         (with-cache-home
          directory
          (lambda ()
            (copy-file (fixture "compressed" "widget.el.gz")
                       (string-append directory "/lib.el.gz"))
            (let* ((root (string-append "file://" directory))
                   (here (string-append root "/here.el"))
                   (result (session-when-ready (list (initialize-with-root 1 root)
                                                     (open-at here "(widget-size 2)\n"
                                                              #:language "emacs-lisp")
                                                     (definition-request 2 here 0 2))))
                   (locations (json-ref (answer-to (cdr result) 2) "result"))
                   (answer (and (vector? locations)
                             (> (vector-length locations) 0)
                             (vector-ref locations 0))))
              (assert-true (and answer #t)
                #:name "the compressed file was indexed")
              (when answer
                (assert-equal #:expect (string-append root "/lib.el.gz")
                  #:got (json-ref answer "uri")
                  #:name "the jump names the compressed file itself"))))))))

    (test "a name the project does not define is found in the standard library"
      ;; The library is a directory of source like any other, handed over by
      ;; the setting a reader who unpacked one somewhere would use.
      (with-temp-directory
       (lambda (directory)
         (with-cache-home
          directory
          (lambda ()
            (let ((project (string-append directory "/project"))
                  (library (string-append directory "/library")))
              (spit (string-append library "/probe.scm")
                    "(define (make-widget size)\n  (* size 2))\n")
              (spit (string-append project "/here.scm")
                    "(make-widget 2)\n")
              (let* ((root (string-append "file://" project))
                     (here (string-append root "/here.scm"))
                     (result (session-when-ready
                              (list (initialize-with-root
                                     1 root #:stdlib (list->vector (list library)))
                                    (open-at here "(make-widget 2)\n")
                                    (definition-request 2 here 0 2))))
                     (locations (json-ref (answer-to (cdr result) 2) "result"))
                     (answer (and (vector? locations)
                               (> (vector-length locations) 0)
                               (vector-ref locations 0))))
                (assert-true (and answer #t)
                  #:name "the name the project does not define was found")
                (when answer
                  (assert-equal #:expect (string-append "file://" library "/probe.scm")
                    #:got (json-ref answer "uri")
                    #:name "in the library, not in the project")))))))))

    (test "the highlights mark the declaration apart from the uses"
      (let* ((result (session (list (initialize-request 1)
                                    (open "(define (f x)\n  (+ f 1))\n")
                                    (highlight-request 2 buffer-uri 1 5))))
             (highlights (vector->list
                          (json-ref (answer-to (cdr result) 2) "result"))))
        (assert-equal #:expect 2 #:got (length highlights))
        (assert-equal #:expect 3 #:got (json-ref (car highlights) "kind")
          #:name "the declaration is a write")
        (assert-equal #:expect 2 #:got (json-ref (cadr highlights) "kind")
          #:name "the use is a read")))

    (test "the outline nests a definition inside the one that holds it"
      (let* ((result (session (list (initialize-request 1)
                                    (open "(define (f)\n  (define (g) 1)\n  (g))\n")
                                    (symbols-request 2 buffer-uri))))
             (symbols (json-ref (answer-to (cdr result) 2) "result"))
             (outer (vector-ref symbols 0))
             (children (json-ref outer "children")))
        (assert-equal #:expect 1 #:got (vector-length symbols))
        (assert-equal #:expect "f" #:got (json-ref outer "name"))
        (assert-equal #:expect 1 #:got (vector-length children)
          #:name "`g' is inside `f', not beside it")
        (assert-equal #:expect "g"
          #:got (json-ref (vector-ref children 0) "name"))))

    (test "the hover describes the symbol rather than the line"
      (let* ((text "(define (square x)\n  \"Square X.\"\n  (* x x))\n\n(define (area r)\n  (square r))\n")
             (result (session (list (initialize-request 1)
                                    (open text)
                                    (hover-at-request 2 buffer-uri 5 4))))
             (value (json-ref (json-ref (answer-to (cdr result) 2) "result")
                              "contents")))
        (assert-contains #:expect (json-ref value "value") #:got "**square**")
        (assert-contains #:expect (json-ref value "value") #:got "function")
        (assert-contains #:expect (json-ref value "value") #:got "Square X."
          #:name "the definition's own docstring, not the rule's")))

    (test "completion offers the names the project defines"
      (let* ((result (session (list (initialize-request 1)
                                    (open-at lib-uri lib-source)
                                    (open "(define (area r)\n  (square r))\n")
                                    (completion-at-request 2 buffer-uri 1 0))))
             (items (json-ref (answer-to (cdr result) 2) "result"))
             (labels (map (lambda (item) (json-ref item "label"))
                          (vector->list items))))
        (assert-true (and (member "area" labels) #t)
          #:name "from this buffer")
        (assert-true (and (member "square" labels) #t)
          #:name "from the one next to it")))

    (test "prepareRename says what would be renamed, and rename edits every mention"
      (let* ((text "(define (f x) x)\n(f 1)\n")
             (prepared (session (list (initialize-request 1)
                                      (open text)
                                      (prepare-rename-request 2 buffer-uri 1 1))))
             (range (json-ref (answer-to (cdr prepared) 2) "result"))
             (renamed (session (list (initialize-request 1)
                                     (open text)
                                     (rename-request 2 buffer-uri 1 1 "g"))))
             (edit (json-ref (answer-to (cdr renamed) 2) "result"))
             (changes (json-ref edit "documentChanges"))
             (edits (json-ref (vector-ref changes 0) "edits")))
        (assert-equal #:expect "f" #:got (json-ref range "placeholder"))
        (assert-equal #:expect 1 #:got (vector-length changes))
        (assert-equal #:expect 2 #:got (vector-length edits)
          #:name "the definition and the call")
        (assert-equal #:expect "g"
          #:got (json-ref (vector-ref edits 1) "newText"))
        (assert-equal #:expect 1
          #:got (json-ref (json-ref (vector-ref changes 0) "textDocument")
                          "version")
          #:name "the edit is pinned to the version it was computed from")))

    (test "a rename that is not a symbol name is refused, not written"
      (let* ((result (session (list (initialize-request 1)
                                    (open "(define (f x) x)\n(f 1)\n")
                                    (rename-request 2 buffer-uri 1 1 "(bad)"))))
             (answer (answer-to (cdr result) 2)))
        (assert-equal #:expect -32602 #:got (json-ref (json-ref answer "error") "code"))))

    (test "a rename of a binding edits its scope and leaves the other binding alone"
      ;; the `x' of `f' and the `x' of `g' are two names, and renaming one is
      ;; not renaming the other
      (let* ((result (session (list (initialize-request 1)
                                    (open (string-append
                                           "(define (f x)\n"
                                           "  (list x x))\n"
                                           "(define (g x) x)\n"))
                                    (rename-request 2 buffer-uri 0 11 "y"))))
             (edit (json-ref (answer-to (cdr result) 2) "result"))
             (changes (json-ref edit "documentChanges"))
             (edits (json-ref (vector-ref changes 0) "edits")))
        (assert-equal #:expect 1 #:got (vector-length changes)
          #:name "the edit stays in the buffer")
        (assert-equal #:expect 3 #:got (vector-length edits)
          #:name "the parameter and its two uses, and nothing from `g'")))

    (test "renaming a Scheme file renames its module and the imports of it"
      (with-temp-directory
       (lambda (directory)
         (with-cache-home
          directory
          (lambda ()
            (spit (string-append directory "/util.scm")
                  "(define-module (app util))\n(define (f) 1)\n")
            (spit (string-append directory "/user.scm")
                  "(import (app util))\n(f)\n")
            (let* ((root (string-append "file://" directory))
                   (old (string-append root "/util.scm"))
                   (new (string-append root "/helpers.scm"))
                   (result (session-when-ready (list (initialize-with-root 1 root)
                                                     (rename-file-request 2 old new))))
                   (edit (json-ref (answer-to (cdr result) 2) "result"))
                   (changes (json-ref edit "documentChanges")))
              (define (replacement-for uri)
                (let loop ((at 0))
                  (if (>= at (vector-length changes))
                    #f
                    (let ((change (vector-ref changes at)))
                      (if (equal? (json-ref (json-ref change "textDocument") "uri")
                                  uri)
                        (json-ref (vector-ref (json-ref change "edits") 0)
                                  "newText")
                        (loop (+ at 1)))))))
              (assert-equal #:expect "helpers" #:got (replacement-for new)
                #:name "the renamed file's module names the new file")
              (assert-equal #:expect "helpers"
                #:got (replacement-for (string-append root "/user.scm"))
                #:name "and the file that imports it names the new module")))))))

    (test "renaming an Emacs Lisp file renames its provide and the requires"
      (with-temp-directory
       (lambda (directory)
         (with-cache-home
          directory
          (lambda ()
            (spit (string-append directory "/old.el") "(provide 'old-feature)\n")
            (spit (string-append directory "/user.el") "(require 'old-feature)\n")
            (let* ((root (string-append "file://" directory))
                   (old (string-append root "/old.el"))
                   (new (string-append root "/helpers.el"))
                   (result (session-when-ready (list (initialize-with-root 1 root)
                                                     (rename-file-request 2 old new))))
                   (edit (json-ref (answer-to (cdr result) 2) "result"))
                   (changes (json-ref edit "documentChanges")))
              (define (replacement-for uri)
                (let loop ((at 0))
                  (if (>= at (vector-length changes))
                    #f
                    (let ((change (vector-ref changes at)))
                      (if (equal? (json-ref (json-ref change "textDocument") "uri")
                                  uri)
                        (json-ref (vector-ref (json-ref change "edits") 0)
                                  "newText")
                        (loop (+ at 1)))))))
              (assert-equal #:expect "helpers" #:got (replacement-for new)
                #:name "the renamed file provides the new feature")
              (assert-equal #:expect "helpers"
                #:got (replacement-for (string-append root "/user.el"))
                #:name "and the file that requires it asks for the new one")))))))

    (test "renaming a Common Lisp file renames no package"
      (with-temp-directory
       (lambda (directory)
         (with-cache-home
          directory
          (lambda ()
            (spit (string-append directory "/old.lisp")
                  "(defpackage #:old (:use #:cl))\n")
            (let* ((root (string-append "file://" directory))
                   (old (string-append root "/old.lisp"))
                   (new (string-append root "/helpers.lisp"))
                   (result (session-when-ready (list (initialize-with-root 1 root)
                                                     (rename-file-request 2 old new)))))
              (assert-equal #:expect json-null
                #:got (json-ref (answer-to (cdr result) 2) "result")
                #:name "a package is not a file, so there is nothing to rename")))))))

    (test "a jump in a read project answers in milliseconds"
      (with-temp-directory
       (lambda (directory)
         (let loop ((n 0))
           (when (< n 400)
             (spit (string-append directory (format #f "/file-~a.scm" n))
                   (format #f "(define (proc-~a x)\n  x)\n" n))
             (loop (+ n 1))))
         (let* ((paths (files-under directory))
                (entries (filter identity (map index-file paths)))
                (workspace (new-workspace))
                (here (path->uri (string-append directory "/file-0.scm")))
                (text "(proc-7 1)\n"))
           (publish-workspace-index! workspace (index-of-files entries 1)
                                     'ready (length entries) (length entries))
           (let* ((start (get-internal-real-time))
                  (locations (lsp-definition workspace (make-hash-table) here text
                                             'scheme 3
                                             #:deadline (make-deadline 100)))
                  (elapsed (milliseconds-since start)))
             (assert-true (< elapsed 100)
               #:name (format #f "answered in ~a ms, under the deadline" elapsed))
             (assert-equal #:expect 1 #:got (vector-length locations))
             (assert-contains
               #:expect (json-ref (vector-ref locations 0) "uri")
               #:got "file-7.scm"
               #:name "and it names the file that defines the name"))))))

    (test "a jump answers from the part of the project that is read"
      (with-temp-directory
       (lambda (directory)
         (let loop ((n 0))
           (when (< n 400)
             (spit (string-append directory (format #f "/file-~a.scm" n))
                   (format #f "(define (proc-~a x)\n  x)\n" n))
             (loop (+ n 1))))
         (let* ((paths (take (files-under directory) 20))
                (entries (filter identity (map index-file paths)))
                (workspace (new-workspace))
                (here (path->uri (string-append directory "/file-0.scm")))
                (text "(proc-100 1)\n"))
           (publish-workspace-index! workspace (index-of-files entries 1)
                                     'partial (length entries) 400)
           (let* ((start (get-internal-real-time))
                  (read (lsp-definition workspace (make-hash-table) here text
                                        'scheme 3
                                        #:deadline (make-deadline 100)))
                  (elapsed (milliseconds-since start))
                  (missing (lsp-definition workspace (make-hash-table) here
                                           "(proc-1 1)\n" 'scheme 3
                                           #:deadline (make-deadline 100))))
             ;; The scan of 400 files has read 20 of them: the jump must answer
             ;; about those twenty and say nothing about the rest, not wait.
             (assert-true (< elapsed 100)
               #:name (format #f "answered in ~a ms from the files read so far"
                              elapsed))
             (assert-equal #:expect 1 #:got (vector-length read))
             (assert-contains #:expect (json-ref (vector-ref read 0) "uri")
               #:got "file-100.scm"
               #:name "in the file that was read")
             (assert-equal #:expect 0 #:got (vector-length missing)
               #:name "a file not read yet is not guessed at"))))))

    (test "a jump is answered while a changed buffer is being checked"
      (with-temp-directory
       (lambda (directory)
         (with-cache-home
          directory
          (lambda ()
            (spit (string-append directory "/lib.scm")
                  "(define (target x)\n  x)\n")
            (let* ((root (string-append "file://" directory))
                   (here (string-append root "/here.scm"))
                   (big (string-append "(target 1)\n"
                                       (apply string-append
                                              (map (lambda (n)
                                                     (format #f "(define (v-~a) ~a)\n"
                                                             n n))
                                                   (iota 2000)))))
                   (result (session-when-ready
                            (list (initialize-with-root 1 root)
                                  (open-at here "(target 1)\n")
                                  (change-at here big)
                                  (definition-request 2 here 0 3))))
                   (locations (json-ref (answer-to (cdr result) 2) "result")))
              (assert-true (and (vector? locations) (> (vector-length locations) 0))
                #:name "the jump still found the project's definition")
              (assert-contains
                #:expect (json-ref (vector-ref locations 0) "uri")
                #:got "lib.scm"
                #:name "in the file that defines it")))))))

    (test "workspace/symbol searches the buffers the editor has open"
      (let* ((result (session (list (initialize-request 1)
                                    (open-at lib-uri lib-source)
                                    (workspace-symbol-request 2 "squ"))))
             (symbols (json-ref (answer-to (cdr result) 2) "result")))
        (assert-equal #:expect 1 #:got (vector-length symbols))
        (assert-equal #:expect "square"
          #:got (json-ref (vector-ref symbols 0) "name"))))))
