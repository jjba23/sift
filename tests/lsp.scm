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
;; Nothing here waits for the process to be ready: the messages are written to
;; its input before it starts, and it answers them in order, because the
;; transport is a stream and the order is the protocol.

(define-module (tests lsp)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (veritas veritas)
  #:use-module (sift json)
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

(define (frame value)
  "Return @var{value} as one framed JSON-RPC message."
  (let* ((body (json-write value))
         (length (bytevector-length (string->utf8 body))))
    (string-append "Content-Length: " (number->string length) "\r\n\r\n"
                   body)))

(define (unframe text)
  "Return the messages in @var{text}, a run of framed messages."
  (let loop ((index 0) (messages '()))
    (let ((header-end (string-contains text "\r\n\r\n" index)))
      (if (not header-end)
        (reverse messages)
        (let* ((header (substring text index header-end))
               (colon (string-index header #\:))
               (length (string->number
                        (string-trim (substring header (+ colon 1)))))
               (start (+ header-end 4))
               (end (min (string-length text) (+ start length)))
               (body (substring text start end)))
          (loop end (cons (json-read body) messages)))))))

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

(define (initialize-request id)
  (request id "initialize"
           (json-object (field "capabilities" (json-object)))))

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

(define (workspace-symbol-request id query)
  (request id "workspace/symbol" (json-object (field "query" query))))

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
          #:got (json-ref capabilities "textDocumentSync")
          #:name "a change replaces the whole buffer")
        (assert-equal #:expect 'true
          #:got (json-ref capabilities "documentFormattingProvider")
          #:name "the server formats")
        (assert-equal #:expect #("quickfix" "refactor.rewrite"
                                 "source.fixAll" "source.fixAll.sift")
          #:got (json-ref (json-ref capabilities "codeActionProvider")
                          "codeActionKinds")
          #:name "the server offers fixes and the one rewrite it can make")
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

    (test "an unknown request is refused, and the server keeps talking"
      (let* ((result (session (list (initialize-request 1)
                                    (open unclean)
                                    (request 5 "textDocument/signatureHelp"
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

    (test "workspace/symbol searches the buffers the editor has open"
      (let* ((result (session (list (initialize-request 1)
                                    (open-at lib-uri lib-source)
                                    (workspace-symbol-request 2 "squ"))))
             (symbols (json-ref (answer-to (cdr result) 2) "result")))
        (assert-equal #:expect 1 #:got (vector-length symbols))
        (assert-equal #:expect "square"
          #:got (json-ref (vector-ref symbols 0) "name"))))))
