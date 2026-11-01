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
;;   textDocument/didSave              re-read the project, check it all again
;;   workspace/didChangeWatchedFiles   the same, for a file nobody has open
;;   workspace/didChangeConfiguration  read the settings again, check again
;;   textDocument/codeAction           each finding's fix, fix-all, format this definition
;;   textDocument/formatting           format the buffer
;;   textDocument/hover                a symbol's definition, or the rule under it
;;   textDocument/definition           the definition a symbol names
;;   textDocument/references           every mention of it in the project
;;   textDocument/documentHighlight    the mentions in this buffer
;;   textDocument/documentSymbol       the outline of the buffer
;;   workspace/symbol                  search those outlines
;;   textDocument/completion           the names the project defines
;;   textDocument/inlayHint            a parameter's name beside its argument
;;   textDocument/prepareRename        whether a symbol can be renamed here
;;   textDocument/rename               rename it, by name, in the project
;;
;; The navigation requests are answered from the same tree the linter reads,
;; with `(sift definitions)' resolving what is in scope at the cursor: a
;; parameter, a `let' variable, an internal definition, and then the file and
;; the rest of the workspace, in that order.  A name is matched as a name, per
;; dialect -- no macro is expanded, no module import is followed -- which is
;; the same promise the linter makes, and the manual says so where it lists
;; what the server answers.
;;
;; What it does not do is as deliberate: no signature help, no type hierarchy,
;; no hover that guesses at a type.  A server that answered every request an
;; editor can make would be a server that lies about half of them.
;;
;; Everything the server answers is configurable, in the same place as the
;; rest of sift: `inlay-hints', `hover' and `completion' are settings like
;; `line-length' is, so a project can set them in its `sift.yaml'.  They can
;; also be set by the editor itself -- `initializationOptions' when the
;; session starts, and `eglot-workspace-configuration' or a `settings.json'
;; through `workspace/didChangeConfiguration' -- and those settings are read by
;; the same validator and layered over the files, because what the editor says
;; is the most specific thing anyone can say about this session.
;;
;; Documents are kept whole and replaced whole (`change: 1' in the sync
;; options).  An incremental edit would mean patching a string by character
;; offsets that the editor computed in UTF-16, and the formatter's promise --
;; read everything, write everything back -- is only kept if the text it reads
;; is the text the editor has.
;;
;; A finding is a function of the text *and of the project*, and most of what
;; this server does is arranged around that: a save and a watched file refresh
;; the index that jumps and references read, a changed configuration is read
;; again and every open buffer is re-checked against it, and the findings of
;; every open buffer -- not only the one that changed -- go out again, because
;; a finding can name a definition in a file nobody is looking at.  What the
;; server asks of the client is the half of that it cannot do alone:
;; `openClose', a full `change', and `save', which is the news that the file on
;; disk is the buffer.
;;
;; It also says what it is doing, over `window/logMessage': the root it serves,
;; the dialect each buffer is read as, the findings each report carries, and
;; every re-read it was asked for.  Eglot shows those lines in the *EGLOT
;; events* buffer, and without them "nothing to report" and "not running" look
;; exactly alike.

(define-module (sift lsp)
  ;; The server is not declarative: it hands the definition accessors to
  ;; `map' and keeps them beside the documents they describe, and a record
  ;; accessor from a declarative neighbour is only inlinable, not a value.
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift lint)
  #:use-module (sift format)
  #:use-module (sift config)
  #:use-module (sift files)
  #:use-module (sift definitions)
  #:use-module (sift imports)
  #:use-module (sift check base)
  #:use-module (sift json)
  #:use-module (sift render)
  #:export (lsp-serve
            lsp-diagnostics
            lsp-code-actions
            lsp-formatting
            lsp-definition
            lsp-references
            lsp-document-symbols))

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
  (make-document uri text language-id version dialect published)
  document?
  (uri document-uri)
  (text document-text set-document-text!)
  (language-id document-language-id)
  (version document-version set-document-version!)
  (dialect document-dialect)
  ;; what the editor was last told, and about which text, as
  ;; `(diagnostics . text)' -- see `publish!' for why the text is kept beside
  ;; the findings rather than the findings alone
  (published document-published set-document-published!))

(define (new-document uri text language-id version dialect)
  "Return a document about which nothing has been published yet."
  (make-document uri text language-id version dialect #f))

(define (client-version value)
  "Return @var{value} as a document version, or @code{#f} when it is not one.

The protocol says a version is an integer, and a client that sends a JSON
@code{null} -- or a string, or nothing at all -- is a client sift should keep
answering rather than one whose document is stored under a version no
comparison can use."
  (and (number? value) value))

(define (dialect-of language-id uri)
  "Return the dialect a buffer is read as.

The client's language identifier is the first word; the file's own extension
answers when the identifier is something sift has never seen -- and when it is
not a string at all, which is what a client that has no language for the file
sends.  A server that raised here would have no document to answer about for
the rest of the session."
  (or (and (string? language-id) (string->dialect language-id))
    (and (string? uri) (detect-dialect #:file (strip-uri uri)))
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

;;; ===== The workspace =====
;;;
;;; Go to definition is a question about more than one file: a function used
;;; in a buffer is usually defined in a file that is not open, and a server
;;; that only looked at the buffer would answer "not here" to half the
;;; questions an editor asks.  So the files under the workspace root are read
;;; once, lazily, on the first request that needs them: their definitions are
;;; what a jump, an outline and a rename all need, and keeping them out of the
;;; buffer being typed keeps the cost at one read per file per session.
;;;
;;; A buffer the editor holds is the authority on its own text, so the index is
;;; only asked about files the editor is not showing -- which is what makes a
;;; jump from an unsaved buffer to an unsaved buffer work.

(define index-file-limit 2000)
(define index-byte-limit 500000)

(define-record-type <workspace>
  (make-workspace root files scanned? filter)
  workspace?
  (root workspace-root set-workspace-root!)
  (files workspace-files)
  (scanned? workspace-scanned? set-workspace-scanned!)
  (filter workspace-filter set-workspace-filter!))

(define* (new-workspace #:optional (config #f))
  (make-workspace #f (make-hash-table) #f
                  (and config (config-ref config 'file-filter))))

(define-record-type <indexed-file>
  (make-indexed-file path text dialect definitions names)
  indexed-file?
  (path indexed-file-path)
  (text indexed-file-text set-indexed-file-text!)
  (dialect indexed-file-dialect)
  (definitions indexed-file-definitions set-indexed-file-definitions!)
  (names indexed-file-names))

;; A file is indexed lazily: the scan reads nothing but its name and its
;; metadata, and the text and the definitions arrive the first time a request
;; needs them.  That is what makes the cache worth having -- a session that
;; only asks for completion reads one small file per workspace, not the
;; workspace -- and what keeps a jump honest, because the file it needs is
;; parsed from the text on disk at the moment it is asked for.

(define (indexed-file-text-of entry)
  "Return the text of @var{entry}, reading the file when it is not loaded."
  (or (indexed-file-text entry)
    (let ((text (read-file-text (indexed-file-path entry))))
      (when text (set-indexed-file-text! entry text))
      text)))

(define (indexed-file-definitions-of entry)
  "Return the definitions of @var{entry}, parsing them when they are not read."
  (or (indexed-file-definitions entry)
    (let ((text (indexed-file-text-of entry)))
      (and text
        (let ((definitions (parse-file-text text (indexed-file-dialect entry)
                                            (indexed-file-path entry))))
          (set-indexed-file-definitions! entry definitions)
          definitions)))))

(define (path->uri path)
  "Return the @code{file:} URI that names @var{path}."
  (format #f "file://~a" (percent-encode path)))

(define (percent-encode text)
  "Return @var{text} with everything outside the URI path alphabet escaped."
  (call-with-output-string
    (lambda (out)
      (string-for-each
       (lambda (character)
         (if (or (char-alphabetic? character) (char-numeric? character)
               (memv character '(#\/ #\- #\_ #\. #\~ #\+)))
           (display character out)
           (let ((hex (string-upcase (number->string (char->integer character) 16))))
             (display #\% out)
             (when (< (string-length hex) 2) (display #\0 out))
             (display hex out))))
       text))))

(define (directory? path)
  (eq? 'directory (false-if-exception (stat:type (stat path)))))

(define* (files-under root #:optional (filter #f))
  "Return the Lisp files under @var{root}, at most `index-file-limit' of them.

Directories a build leaves behind are skipped rather than walked: `.git'
alone holds more files than most source trees, and reading it would make the
first definition request slower than finding the file by hand."
  (let loop ((pending (list root)) (found '()) (count 0))
    (cond
      [(or (null? pending) (>= count index-file-limit)) (reverse found)]
      [else
        (let* ((dir (car pending))
               (entries (or (false-if-exception (scandir dir)) '()))
               (subdirectories '())
               (files '()))
          (for-each
           (lambda (name)
             (unless (or (member name skipped-directories)
                       (string-prefix? "." name))
               (let ((path (string-append dir "/" name)))
                 (cond
                   [(directory? path) (set! subdirectories (cons path subdirectories))]
                   [(file-included? filter path root)
                    (set! files (cons path files))]))))
           entries)
          (loop (append subdirectories (cdr pending))
                (append files found)
                (+ count (length files))))])))

(define (read-file-text path)
  "Return the text of @var{path}, or @code{#f} when it is not a file to read.

A file that cannot be read, or is larger than a file anyone edits by hand, is
left out: an editor asking for a definition should not wait while the server
reads a generated file nobody edits."
  (let ((text (false-if-exception (call-with-input-file path get-string-all))))
    (and (string? text) (< (string-length text) index-byte-limit) text)))

(define (parse-file-text text dialect path)
  "Return the definitions of @var{text}, read as @var{dialect}."
  (parse-definitions
   (call-with-values
     (lambda () (read-source #:source text #:dialect dialect #:file path))
     (lambda (ast errors) ast))))

(define (top-level-names definitions)
  "Return the names @var{definitions} define at the top level, without repeats."
  (delete-duplicates
   (map definition-name
        (filter (lambda (definition) (not (definition-scope definition)))
                definitions))))

(define (index-file path)
  "Return @var{path} as a fully read `indexed-file', or @code{#f}.

Used for a file the editor just saved or a watcher just reported: the answer
has to be right now, and a lazy entry would only defer the work by a moment."
  (let ((dialect (detect-dialect #:file path))
        (text (read-file-text path)))
    (and dialect text
      (let ((definitions (parse-file-text text dialect path)))
        (make-indexed-file path text dialect definitions
                           (top-level-names definitions))))))

(define cache-version 1)

(define (cache-home)
  "Return the directory sift keeps its caches in, or @code{#f} when it has none."
  (let ((xdg (getenv "XDG_CACHE_HOME"))
        (home (getenv "HOME")))
    (cond
      [(and xdg (not (string-null? xdg))) (string-append xdg "/sift")]
      [(and home (not (string-null? home))) (string-append home "/.cache/sift")]
      [else #f])))

(define (path-hash text)
  "Return a short, deterministic name for @var{text}, for a cache file.

FNV-1a, because a cache file's name has to be the same on the next run and a
hash table's order is not a promise about anything; the path is also stored
inside the file, so a collision costs a reread and never a wrong answer.

The exclusive or is `logxor', which is what Guile calls it: `bitwise-xor' is
not a binding in this Guile at all, and a name that only looks like the right
one raises the moment a project has a root to cache.  That would be the whole
of navigation -- definition, references, completion, rename -- failing on the
first request of a real editor session, which is why this function is tested
against a workspace rather than only against itself."
  (let loop ((characters (string->list text)) (hash 14695981039346656037))
    (if (null? characters)
      (number->string hash 16)
      (loop (cdr characters)
            (modulo (* (logxor hash (char->integer (car characters)))
                       1099511628211)
                    18446744073709551616)))))

(define (cache-file root)
  (let ((directory (cache-home)))
    (and directory (format #f "~a/index-~a.json" directory (path-hash root)))))

(define (read-cache root)
  "Return the cached entries for @var{root}, as a hash of path to metadata.

Each entry is @code{(mtime size dialect names)}.  A cache that cannot be read
-- an older layout, a truncated file, a value that is not an object -- is no
cache at all: the workspace is read again, which is slower and never wrong."
  (let ((path (cache-file root))
        (entries (make-hash-table)))
    (when (and path (file-exists? path))
      (let ((document (false-if-exception
                       (call-with-input-file path
                                             (lambda (port) (json-read (get-string-all port)))))))
        (when (and (json-object? document)
                (equal? (json-ref document "version") cache-version)
                (equal? (json-ref document "root") root))
          (for-each
           (lambda (entry)
             (let ((file (json-ref entry "path" #f)))
               (when file
                 (hash-set! entries file
                            (list (json-ref entry "mtime" 0)
                                  (json-ref entry "size" 0)
                                  (json-ref entry "dialect" "scheme")
                                  (map (lambda (name) name)
                                       (vector->list
                                        (json-ref entry "names" (vector)))))))))
           (vector->list (json-ref document "files" (vector)))))))
    entries))

(define (make-cache-directory directory)
  "Create @var{directory} and every missing parent, like @code{mkdir -p}."
  (unless (or (file-exists? directory) (string=? directory "/")
            (string-null? directory))
    (make-cache-directory (dirname directory))
    (false-if-exception (mkdir directory))))

(define (write-cache! root workspace)
  "Write the metadata of @var{workspace} under @var{root}, when it can be.

What is written is what the next session can trust without reading: the path,
the size and the modification time that decide whether the file changed, the
dialect, and the names it defines.  A cache is an optimisation, so a failure
to write one is not a failure of the session."
  (let ((target (cache-file root)))
    (when target
      (make-cache-directory (dirname target))
      (false-if-exception
       (call-with-output-file target
                              (lambda (port)
                                (display
                                 (json-write
                                  (json-object
                                   (field "version" cache-version)
                                   (field "root" root)
                                   (field "files"
                                          (list->vector
                                           (map (lambda (entry)
                                                  (let* ((file (indexed-file-path entry))
                                                         (attributes (false-if-exception (stat file))))
                                                    (json-object
                                                     (field "path" file)
                                                     (field "mtime" (if attributes (stat:mtime attributes) 0))
                                                     (field "size" (if attributes (stat:size attributes) 0))
                                                     (field "dialect"
                                                            (symbol->string (indexed-file-dialect entry)))
                                                     (field "names"
                                                            (list->vector (indexed-file-names entry))))))
                                                (hash-map->list (lambda (path entry) entry)
                                                                (workspace-files workspace)))))))
                                 port)))))))

(define (workspace-index workspace)
  "Return the workspace's index, scanning the tree the first time it is asked.

The scan reads the names of the files and nothing else: the text and the
definitions of a file arrive when a request needs them.  A file whose size and
modification time are the same as the last session's starts from the names the
cache remembers, and is parsed the moment something asks for more."
  (unless (workspace-scanned? workspace)
    (let* ((root (workspace-root workspace))
           (cached (if root (read-cache root) (make-hash-table))))
      (when root
        (for-each
         (lambda (path)
           (let ((remembered (hash-ref cached path))
                 (dialect (detect-dialect #:file path)))
             (when dialect
               (let ((attributes (false-if-exception (stat path))))
                 (if (and remembered attributes
                       (= (car remembered) (stat:mtime attributes))
                       (= (cadr remembered) (stat:size attributes))
                       (string=? (caddr remembered) (symbol->string dialect)))
                   (hash-set! (workspace-files workspace) path
                              (make-indexed-file path #f dialect #f
                                                 (cadddr remembered)))
                   (let ((entry (index-file path)))
                     (when entry
                       (hash-set! (workspace-files workspace) path entry))))))))
         (files-under root (workspace-filter workspace)))
        (write-cache! root workspace)))
    (set-workspace-scanned! workspace #t))
  (workspace-files workspace))

(define (workspace-refresh! workspace path)
  "Read @var{path} again, or forget it when it is gone or no longer wanted.

A file the editor saved, and a file a watcher reported, are the two ways a
project changes under a running server.  A file that is no longer one of the
project's -- deleted, or excluded by a filter the project has since changed --
is forgotten rather than kept as a stale answer."
  (let* ((root (workspace-root workspace))
         (wanted? (and root (file-exists? path)
                    (file-included? (workspace-filter workspace) path root))))
    (if wanted?
      (let ((entry (index-file path)))
        (if entry
          (hash-set! (workspace-files workspace) path entry)
          (hash-remove! (workspace-files workspace) path)))
      (hash-remove! (workspace-files workspace) path))))

(define (workspace-definition-sites workspace name dialect)
  "Return @code{(uri text definition)} for every file-level definition of @var{name}."
  (let ((sites '()))
    (hash-for-each
     (lambda (path entry)
       (when (and (eq? (indexed-file-dialect entry) dialect)
               (member name (indexed-file-names entry)))
         (for-each
          (lambda (definition)
            (when (and (not (definition-scope definition))
                    (string=? (definition-name definition) name))
              (set! sites
                    (cons (list (path->uri path) (indexed-file-text-of entry)
                                definition)
                          sites))))
          (indexed-file-definitions-of entry))))
     (workspace-index workspace))
    (reverse sites)))

(define (workspace-names workspace dialect)
  "Return the names the workspace defines in @var{dialect}, without repeats."
  (let ((names '()))
    (hash-for-each
     (lambda (path entry)
       (when (eq? (indexed-file-dialect entry) dialect)
         (set! names (append (indexed-file-names entry) names))))
     (workspace-index workspace))
    (delete-duplicates (reverse names))))

(define (document-definition-sites documents name dialect except-uri)
  "Return open buffers that define @var{name}, skipping @var{except-uri}."
  (let ((sites '()))
    (hash-for-each
     (lambda (uri document)
       (when (and (not (string=? uri except-uri))
               (eq? (document-dialect document) dialect))
         (for-each
          (lambda (definition)
            (set! sites
                  (cons (list uri (document-text document) definition) sites)))
          (filter (lambda (definition)
                    (not (definition-scope definition)))
                  (definitions-by-name
                   (ast-of-text (document-text document) dialect) name)))))
     documents)
    (reverse sites)))

(define (ast-of-text text dialect)
  "Return the AST of @var{text}, read as @var{dialect}."
  (call-with-values
    (lambda () (read-source #:source text #:dialect dialect))
    (lambda (ast errors) ast)))

(define (form-start node) (source-location-index (node-start node)))
(define (form-end node) (source-location-index (node-end node)))

;;; ===== Navigation =====
;;;
;;; Each answer is a function of the text, the cursor and the project, and none
;;; of them is cached: a jump computed from the buffer the editor has is right
;;; even when the buffer is unsaved, which is the state a jump is asked for
;;; most often.  The order is always the same, and it is the order a reader
;;; would look in: what is in scope here, then this file, then the project.

(define (definition-location uri text definition)
  (json-object
   (field "uri" uri)
   (field "range" (json-range text (definition-index definition)
                              (definition-end definition)))))

(define (definition-sites-in ast workspace documents uri text dialect index)
  "Return every place the symbol at @var{index} is defined, in the project.

The AST is passed in rather than read here, because a request that asks the
same question about many positions -- inlay hints ask it once per call in the
range the editor is showing -- must parse the buffer once, not once per
question."
  (let ((local (resolve-definition ast index)))
    (cond
      [local (list (list uri text local))]
      [else
        (let ((name (resolve-name ast index)))
          (if (not name)
            '()
            (append (document-definition-sites documents name dialect uri)
                    (workspace-definition-sites workspace name dialect))))])))

(define (definition-sites workspace documents uri text dialect index)
  "Return every place the symbol at @var{index} is defined, in the project."
  (definition-sites-in (ast-of-text text dialect) workspace documents
                       uri text dialect index))

(define* (lsp-definition workspace documents uri text dialect index)
  "Return the locations the symbol at @var{index} is defined at, as an array."
  (list->vector
   (map (lambda (site)
          (definition-location (car site) (cadr site) (caddr site)))
        (definition-sites workspace documents uri text dialect index))))

(define (project-texts workspace documents uri text dialect)
  "Return @code{(uri text)} for the buffer, the other buffers and the project."
  (let ((sites (list (list uri text))))
    (hash-for-each
     (lambda (other-uri document)
       (when (and (not (string=? other-uri uri))
               (eq? (document-dialect document) dialect))
         (set! sites (cons (list other-uri (document-text document)) sites))))
     documents)
    (hash-for-each
     (lambda (path entry)
       (let ((other (path->uri path)))
         (when (and (eq? (indexed-file-dialect entry) dialect)
                 (not (string=? other uri)))
           (set! sites (cons (list other (indexed-file-text-of entry)) sites)))))
     (workspace-index workspace))
    (reverse sites)))

(define* (lsp-references workspace documents uri text dialect index
                         #:key (include-declaration #t))
  "Return every location in the project that mentions the symbol at @var{index}."
  (let* ((ast (ast-of-text text dialect))
         (name (resolve-name ast index)))
    (if (not name)
      (vector)
      (let ((locations '()))
        (for-each
         (lambda (site)
           (let* ((site-uri (car site))
                  (site-text (cadr site))
                  (site-ast (ast-of-text site-text dialect))
                  (declarations
                   (map (lambda (definition) (definition-index definition))
                        (definitions-by-name site-ast name))))
             (for-each
              (lambda (node)
                (unless (and (not include-declaration)
                          (memv (form-start node) declarations))
                  (set! locations
                        (cons (json-object
                               (field "uri" site-uri)
                               (field "range" (json-range site-text
                                                          (form-start node)
                                                          (form-end node))))
                              locations))))
              (occurrences-of site-ast name))))
         (project-texts workspace documents uri text dialect))
        (list->vector (reverse locations))))))

(define (lsp-document-highlights text dialect index)
  "Return every mention of the symbol at @var{index} in @var{text}."
  (let* ((ast (ast-of-text text dialect))
         (name (resolve-name ast index)))
    (if (not name)
      (vector)
      (let ((declarations
             (map (lambda (definition) (definition-index definition))
                  (definitions-by-name ast name))))
        (list->vector
         (map (lambda (node)
                (json-object
                 (field "range" (json-range text (form-start node) (form-end node)))
                 ;; 2 is a read, 3 a write: the declaration is where the name
                 ;; is written, and its mentions are where it is read.
                 (field "kind" (if (memv (form-start node) declarations) 3 2))))
              (occurrences-of ast name)))))))

(define (symbol-detail definition text)
  "Return the first line of a definition's form, as an outline shows it."
  (let* ((form (definition-form definition))
         (start (form-start form))
         (end (min (form-end form) (string-length text)))
         (snippet (substring text start end))
         (first-line (string-trim-both (car (string-split snippet #\newline)))))
    (if (> (string-length first-line) 120)
      (string-append (substring first-line 0 119) "…")
      first-line)))

(define (document-symbol-of entry text)
  "Return one outline entry, with the definitions nested inside it."
  (let ((definition (car entry))
        (children (cdr entry)))
    (json-object
     (field "name" (definition-name definition))
     (field "detail" (symbol-detail definition text))
     (field "kind" (symbol-kind (definition-kind definition)))
     (field "range" (json-range text (form-start (definition-form definition))
                                (form-end (definition-form definition))))
     (field "selectionRange" (json-range text (definition-index definition)
                                         (definition-end definition)))
     (field "children"
            (list->vector
             (map (lambda (child) (document-symbol-of child text)) children))))))

(define* (lsp-document-symbols text #:key (dialect 'scheme))
  "Return the outline of @var{text}: every definition, nested."
  (list->vector
   (map (lambda (entry) (document-symbol-of entry text))
        (document-symbol-tree (ast-of-text text dialect)))))

(define (symbol-character? character)
  "Return @code{#t} for a character a Lisp name can be made of."
  (or (char-alphabetic? character)
    (char-numeric? character)
    (memv character '(#\- #\_ #\? #\! #\* #\+ #\/ #\< #\> #\= #\: #\#))))

(define (symbol-prefix-at text index)
  "Return the name being typed before @var{index}, or the empty string.

A completion is almost always asked for in the middle of a half-written name,
and the name is the most specific thing the client and the server both know:
it says which of the project's names the reader is reaching for."
  (let loop ((at (min index (string-length text))) (characters '()))
    (if (or (<= at 0) (not (symbol-character? (string-ref text (- at 1)))))
      (list->string characters)
      (loop (- at 1) (cons (string-ref text (- at 1)) characters)))))

(define (completion-rank name prefix local?)
  "Return a sort key for @var{name} given what is being typed.

Three things decide, in this order: whether the name starts with what is
typed, then whether it is defined in the buffer the reader is in, then the
name itself.  A prefix match is what a reader is almost always asking for, and
a name in this file is nearer than one in a file they cannot see.  Nothing is
inferred and nothing is hidden: a name that does not match at all is still
offered, last, because the client may be completing something sift cannot see
the start of."
  (let ((downcased (string-downcase name))
        (needle (string-downcase prefix)))
    (format #f "~a~a~a"
            (cond
              [(string-null? needle) 1]
              [(string-prefix? needle downcased) 0]
              [(string-contains downcased needle) 1]
              [else 2])
            (if local? 0 1)
            name)))

(define (completion-items workspace documents uri text dialect index)
  "Return the names the buffer and the project define, as ranked items."
  (let* ((local (filter (lambda (definition) (not (definition-scope definition)))
                        (parse-definitions (ast-of-text text dialect))))
         (prefix (symbol-prefix-at text index))
         (kinds (make-hash-table))
         (names '()))
    (define (add name kind local?)
      (unless (hash-ref kinds name)
        (hash-set! kinds name (cons (completion-kind kind) local?)))
      (unless (member name names) (set! names (cons name names))))
    (for-each (lambda (definition)
                (add (definition-name definition) (definition-kind definition) #t))
              local)
    (hash-for-each
     (lambda (other-uri document)
       (when (and (not (string=? other-uri uri))
               (eq? (document-dialect document) dialect))
         (for-each (lambda (definition)
                     (add (definition-name definition) (definition-kind definition) #f))
                   (filter (lambda (definition)
                             (not (definition-scope definition)))
                           (parse-definitions
                            (ast-of-text (document-text document) dialect))))))
     documents)
    (for-each (lambda (name) (add name 'variable #f))
              (workspace-names workspace dialect))
    (list->vector
     (map (lambda (entry)
            (let* ((name (cdr entry))
                   (kind (car (hash-ref kinds name))))
              (json-object (field "label" name)
                           (field "kind" kind)
                           (field "sortText" (car entry)))))
          (sort (map (lambda (name)
                       (cons (completion-rank name prefix
                                              (cdr (hash-ref kinds name)))
                             name))
                     names)
                (lambda (a b) (string<? (car a) (car b))))))))

(define (workspace-symbols workspace documents query dialect)
  "Return the definitions whose name contains @var{query}, as `workspace/symbol'.

@var{dialect} @code{#f} means every dialect, which is what the request means:
`workspace/symbol' names no buffer to take a language from."
  (let ((needle (string-downcase query))
        (entries '()))
    (define (consider uri text definition)
      (let ((name (definition-name definition)))
        (when (string-contains (string-downcase name) needle)
          (set! entries
                (cons (json-object
                       (field "name" name)
                       (field "kind" (symbol-kind (definition-kind definition)))
                       (field "location" (definition-location uri text definition)))
                      entries)))))
    (hash-for-each
     (lambda (uri document)
       (when (or (not dialect) (eq? (document-dialect document) dialect))
         (let ((document-dialect (document-dialect document)))
           (for-each (lambda (definition)
                       (unless (definition-scope definition)
                         (consider uri (document-text document) definition)))
                     (parse-definitions
                      (ast-of-text (document-text document) document-dialect))))))
     documents)
    (hash-for-each
     (lambda (path entry)
       (when (or (not dialect) (eq? (indexed-file-dialect entry) dialect))
         (for-each (lambda (definition)
                     (unless (definition-scope definition)
                       (consider (path->uri path) (indexed-file-text-of entry) definition)))
                   (indexed-file-definitions-of entry))))
     (workspace-index workspace))
    (list->vector (reverse entries))))

(define (valid-symbol-name? name)
  "Return @code{#t} when @var{name} can be written as a Lisp symbol."
  (and (string? name)
    (not (string-null? name))
    (not (string-any (lambda (character)
                       (or (char-whitespace? character)
                         (memv character
                               '(#\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\, #\; #\| #\#))))
                     name))))

(define* (rename-edit workspace documents uri text dialect index new-name
                      #:key (version #f))
  "Return a @code{WorkspaceEdit} renaming the symbol at @var{index} everywhere.

The buffer the rename started in carries the version it was computed from, so
an editor will not apply the edit over typing that arrived in the meantime."
  (let ((name (resolve-name (ast-of-text text dialect) index))
        (changes '()))
    (for-each
     (lambda (site)
       (let* ((site-uri (car site))
              (site-text (cadr site))
              (nodes (occurrences-of (ast-of-text site-text dialect) name)))
         (when (pair? nodes)
           (set! changes
                 (cons (json-object
                        (field "textDocument"
                               (text-document-of site-uri
                                                 (and version
                                                   (string=? site-uri uri)
                                                   version)))
                        (field "edits"
                               (list->vector
                                (map (lambda (node)
                                       (json-object
                                        (field "range"
                                               (json-range site-text
                                                           (form-start node)
                                                           (form-end node)))
                                        (field "newText" new-name)))
                                     nodes))))
                       changes)))))
     (project-texts workspace documents uri text dialect))
    (json-object (field "documentChanges" (list->vector (reverse changes))))))

(define (symbol-hover text dialect definition)
  "Return the hover that describes a definition rather than a rule."
  (let ((dialect (symbol->string dialect)))
    (json-object
     (field "contents"
            (json-object
             (field "kind" "markdown")
             (field "value"
                    (format #f "**~a** — ~a~%~%```~a~%~a~%```~@[~%~%~a~]"
                            (definition-name definition)
                            (definition-kind definition)
                            dialect
                            (symbol-detail definition text)
                            (definition-docstring definition))))))))

(define (documentation-text text)
  "Return a docstring without the quotes the reader kept around it."
  (if (and (string? text) (>= (string-length text) 2)
        (char=? (string-ref text 0) #\")
        (char=? (string-ref text (- (string-length text) 1)) #\"))
    (substring text 1 (- (string-length text) 1))
    text))

(define (call-at ast index)
  "Return the nearest form whose head is a symbol and whose arguments hold
@var{index}, or @code{#f}.

That is what a signature is about: the cursor is inside the arguments of a
call, not on its name and not in the file around it.  The ancestors come
innermost first, so the answer is the first one that qualifies."
  (call-with-values (lambda () (node-path-at ast index))
    (lambda (node ancestors)
      ;; The node the cursor is in is a candidate too: when it is the form's
      ;; own last character -- the space before `)' -- there is no child to
      ;; descend into, and the form is the whole path.
      (find (lambda (candidate)
              (and (node-list? candidate)
                (let ((head (node-head candidate)))
                  (and head (node-atom? head)
                    (< (source-location-index (node-end head)) index)))))
            (cons node ancestors)))))

(define (argument-index form index)
  "Return how many arguments of @var{form} end before @var{index}.

An argument that is complete is an argument the reader has finished, so the
number of them is the index of the one the cursor is in -- including one past
the last, when the reader is starting a new argument, which is the protocol's
way of saying \"the next one\"."
  (let loop ((elements (cdr (node-elements form))) (count 0))
    (cond
      [(null? elements) count]
      [(and (node-data? (car elements))
         (<= (source-location-index (node-end (car elements))) index))
       (loop (cdr elements) (+ count 1))]
      [else count])))

(define* (lsp-signature-help workspace documents uri text dialect index)
  "Return the signature of the call the cursor is in, or @code{#f}.

The signature is the one sift can know: the definition's own formal list, and
the docstring the hover already shows.  A procedure from a library sift cannot
read has no signature here, and answering with an invented one would be worse
than answering nothing."
  (let* ((ast (ast-of-text text dialect))
         (call (call-at ast index)))
    (and call
      (let ((head (node-head call)))
        (let* ((sites (definition-sites workspace documents uri text dialect
                                        (source-location-index (node-start head))))
               (definition (and (pair? sites) (caddr (car sites))))
               (formals (and definition
                          (filter-map (lambda (formal)
                                        (and (node-atom? formal) (node-value formal)))
                                      (definition-formals definition)))))
          (and (pair? formals)
            (let* ((name (definition-name definition))
                   (label (format #f "(~a ~a)" name (string-join formals " ")))
                   (documentation (documentation-text
                                   (definition-docstring definition))))
              (json-object
               (field "signatures"
                      (vector
                       (json-object
                        (field "label" label)
                        (field "parameters"
                               (list->vector
                                (map (lambda (formal)
                                       (json-object (field "label" formal)))
                                     formals)))
                        (field "documentation"
                               (if (string? documentation)
                                 (json-object (field "kind" "markdown")
                                              (field "value" documentation))
                                 json-null)))))
               (field "activeSignature" 0)
               (field "activeParameter" (argument-index call index))))))))))

(define (workspace-root-of params)
  "Return the directory an `initialize' request says the workspace is under."
  (let* ((folders (json-ref params "workspaceFolders" #f))
         (first (and (vector? folders) (> (vector-length folders) 0)
                  (vector-ref folders 0)))
         (uri (or (and first (json-ref first "uri" #f))
                (json-ref params "rootUri" #f))))
    (cond
      [(and (string? uri) (string-prefix? "file://" uri)) (strip-uri uri)]
      [else
        (let ((path (json-ref params "rootPath" #f)))
          (and (string? path) (not (string-null? path)) path))])))

;;; ===== Inlay hints =====
;;;
;;; A hint is a parameter's name written where its argument is, so that
;;; `(make-window ui width 640)' can be read without opening `make-window' to
;;; find out which number is which.  What sift knows is a definition's own
;;; formal list -- the same one signature help shows -- so a call to a
;;; procedure sift cannot read gets no hint at all: an inlay hint is an
;;; assertion about what an argument means, and a guessed one is worse than a
;;; blank space.
;;;
;;; Hints are emitted only for positional arguments.  An argument that is
;;; already a keyword says its own name, and a hint over it would be the
;;; editor repeating what the code says.

(define (argument-name-at formals index)
  "Return the parameter name of the argument at @var{index}, or @code{#f}."
  (and (< index (length formals)) (list-ref formals index)))

(define (positional-arguments form)
  "Return the arguments of @var{form} up to its first keyword, in order.

A call has positional arguments and then keyword ones: once `#:width' is
passed, what follows is the value of a keyword, and the keyword says its own
name.  Hinting `480' in `(make-window ui 640 #:height 480)' with `width' would
be reading the call as if the keyword were not there."
  (let loop ((rest (form-arguments form)) (kept '()))
    (cond
      [(null? rest) (reverse kept)]
      [(keyword-node? (car rest)) (reverse kept)]
      [else (loop (cdr rest) (cons (car rest) kept))])))

(define (inlay-hint label position padding-left?)
  "Return one `InlayHint' for @var{label} at @var{position}.

`kind' 2 is the protocol's \"parameter\": an editor that styles hints shows it
as a parameter name rather than as a type.  `paddingLeft' asks for a space
before the label, so the hint reads as a word beside the argument instead of
as part of it."
  (json-object
   (field "position" position)
   (field "label" label)
   (field "kind" 2)
   (field "paddingLeft" padding-left?)))

(define (call-formals ast workspace documents uri text dialect node)
  "Return the parameter names of the procedure @var{node} calls, or @code{#f}.

The definition is resolved the way a jump resolves it, at the position of the
call's own name, so a hint and a jump never disagree about which `open' the
call means."
  (let* ((head (node-head node))
         (sites (definition-sites-in ast workspace documents uri text dialect
                                     (source-location-index (node-start head))))
         (definition (and (pair? sites) (caddr (car sites)))))
    (and definition
      (let ((formals (filter-map (lambda (formal)
                                   (and (node-atom? formal) (node-value formal)))
                                 (definition-formals definition))))
        (and (pair? formals) formals)))))

(define* (lsp-inlay-hints workspace documents uri text dialect start end)
  "Return the inlay hints for the calls of @var{text} between @var{start} and
@var{end}, as the array an editor expects.

The range decides what is answered, not only what is looked at: a hint is
emitted for an argument that *starts* inside it, which is what an editor asks
for -- the part of the file it is showing -- and which keeps a long file from
being resolved a form at a time for hints nobody will see.

The walk is over forms rather than over every list, because a parameter list
is a list and is not a call: `(define (make-window parent width) ...)' has a
`parent' in argument position that nothing is being passed to, and a hint
there would be sift describing a definition as if it were a call to itself."
  (let ((hints '())
        (ast (ast-of-text text dialect)))
    (for-each-form
     (lambda (node)
       (when (and (call-form? node)
               (let ((head (node-head node)))
                 (and head (node-atom? head)
                   (eq? (node-atom-type head) 'symbol))))
         (let ((arguments (positional-arguments node)))
           ;; the cheap question first: is any of this call's arguments in the
           ;; part of the file the editor is showing?
           (when (any (lambda (argument)
                        (let ((index (source-location-index (node-start argument))))
                          (and (>= index start) (<= index end))))
                      arguments)
             (let ((formals (call-formals ast workspace documents uri text dialect
                                          node)))
               (when formals
                 (let loop ((rest arguments) (position 0))
                   (unless (null? rest)
                     (let* ((argument (car rest))
                            (index (source-location-index (node-start argument)))
                            (name (argument-name-at formals position)))
                       (when (and name (>= index start) (<= index end))
                         (set! hints
                               (cons (inlay-hint name (json-position text index) #t)
                                     hints)))
                       (loop (cdr rest) (+ position 1)))))))))))
     ast)
    (list->vector (reverse hints))))

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
        (cons 'blank-lines (config-ref config 'blank-lines))
        (cons 'single-letter-names (config-ref config 'single-letter-names))
        (cons 'field-macros (config-ref config 'field-macros))
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

(define code-action-kinds
  ;; What `initialize' advertises, and the complete list of what this server
  ;; offers an editor: two kinds of finding fix, the one rewrite that does not
  ;; need to be told what to call anything, and the two source actions.
  '("quickfix" "refactor.rewrite" "source.fixAll" "source.fixAll.sift"
    "source.organizeImports"))

(define (text-document-of uri version)
  "Return the `textDocument' of a workspace edit, with a version when known.

An edit that names the version it was computed from is applied only while the
buffer still is that version, which is the difference between renaming a
symbol and renaming whatever is in that spot now."
  (if (and version (number? version))
    (json-object (field "uri" uri) (field "version" version))
    (json-object (field "uri" uri))))

(define (kind-matches? requested kind)
  "Return @code{#t} when offering @var{kind} satisfies a request for
@var{requested}.

The protocol's rule is a prefix on a whole segment: `source' asks for both
`source.fixAll' actions, and `ref' asks for nothing, because no kind begins
with those three letters and then a dot."
  (or (string=? requested kind)
    (and (string-prefix? requested kind)
      (< (string-length requested) (string-length kind))
      (char=? (string-ref kind (string-length requested)) #\.))))

(define (only-accepts? only kind)
  "Return @code{#t} when a request for @var{only} wants an action of @var{kind}.

No `context.only' means the client has no preference and takes everything."
  (or (not (pair? only))
    (any (lambda (requested) (kind-matches? requested kind)) only)))

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
                      ;; `text-document-of' and not a version written out by
                      ;; hand: a version sift does not know is left out rather
                      ;; than written as JSON `false', which a client compares
                      ;; against the buffer and refuses.
                      (field "textDocument" (text-document-of uri version))
                      (field "edits"
                             (vector
                              (json-object
                               (field "range" (json-range text (car span) (cdr span)))
                               (field "newText" replacement))))))))))))

(define (workspace-edit uri version text start end replacement)
  "Return a `WorkspaceEdit' replacing @var{start}..@var{end} of @var{text}."
  (json-object
   (field "documentChanges"
          (vector
           (json-object
            (field "textDocument" (text-document-of uri version))
            (field "edits"
                   (vector
                    (json-object
                     (field "range" (json-range text start end))
                     (field "newText" replacement)))))))))

(define* (fix-all-action config text #:key (dialect 'scheme) (uri #f) (version #f))
  "Return the action that applies every safe fix in the buffer, or @code{#f}.

The fixes are applied the way `sift fix' applies them -- all of them, from the
highest offset down, and none at all if together they would not read -- and
the answer is one edit for the whole buffer, because a result the buffer can
be reconciled with in one step is the only kind worth offering."
  (let* ((warnings (warnings-for config text #:dialect dialect #:uri uri))
         (fixed (apply-fixes text warnings)))
    (and (not (string=? fixed text))
      (json-object
       (field "title" "sift: fix every safe finding in this buffer")
       (field "kind" "source.fixAll.sift")
       (field "edit"
              (workspace-edit (or uri "") version text 0 (string-length text)
                              fixed))))))

(define (top-level-form-at ast index)
  "Return the form of @var{ast} that contains @var{index}, or @code{#f}."
  (find (lambda (node)
          (and (node-data? node)
            (<= (form-start node) index)
            (< index (max (form-start node) (form-end node)))))
        ast))

(define* (format-form-action config text ast index #:key (dialect 'scheme)
                             (uri #f) (version #f))
  "Return the action that formats the definition the cursor is inside, or #f.

Only a top-level form is offered, because the formatter indents from column
zero: formatting a fragment of a form would move the fragment to the margin
and leave the file worse than it found it.  A whole definition is exactly the
span an author reformats by hand, and it is what \"format this\" means when the
cursor is inside it."
  (let* ((node (top-level-form-at ast index)))
    (and node
      (let* ((start (form-start node))
             (end (form-end node))
             (fragment (substring text start end)))
        (call-with-values
          (lambda ()
            (format-source fragment #:dialect dialect
                           #:file (and uri (strip-uri uri))
                           #:style (config-ref config 'style)
                           #:indent (config-ref config 'indent)
                           #:comment-align (config-ref config 'comment-align)
                           #:brackets (config-ref config 'brackets)
                           #:blank-lines (config-ref config 'blank-lines)
                           #:indent-rules (config-ref config 'indent-rules)))
          (lambda (formatted errors)
            (and (null? errors)
              (not (string=? formatted fragment))
              (json-object
               (field "title" "sift: format this definition")
               (field "kind" "refactor.rewrite")
               (field "edit"
                      (workspace-edit (or uri "") version text start end
                                      formatted))))))))))

(define (import-spec-text text spec)
  "Return @var{spec} as it is written in @var{text}."
  (substring text (form-start spec) (form-end spec)))

(define (import-spec-key spec)
  "Return the name a spec is sorted by: the library it imports."
  (let ((library (spec-library spec)))
    (or (and library (library-key library)) "")))

(define (import-line-end text index)
  "Return the index a deletion that removes a whole line stops at."
  (let loop ((at index))
    (cond
      [(>= at (string-length text)) at]
      [(char=? (string-ref text at) #\newline) (+ at 1)]
      [(memv (string-ref text at) '(#\space #\tab)) (loop (+ at 1))]
      [else index])))

(define (sorted-import-specs specs)
  "Return @var{specs} in the order a reader looks for them: by library name."
  (sort (list-copy specs)
        (lambda (a b) (string<? (import-spec-key a) (import-spec-key b)))))

(define (organize-import-form text form specs)
  "Return @var{form} with its specs put in order, and the layout it had.

The specs are permuted in place -- each one written into the span another one
held -- so a form written on one line stays on one line and a form written one
spec per line keeps its indentation.  Organizing imports is not an invitation
to reflow a file."
  (let ((sorted (sorted-import-specs specs)))
    (if (equal? sorted specs)
      (substring text (form-start form) (form-end form))
      (call-with-output-string
        (lambda (out)
          (let loop ((position (form-start form))
                     (remaining specs)
                     (texts (map (lambda (spec) (import-spec-text text spec)) sorted)))
            (if (null? remaining)
              (display (substring text position (form-end form)) out)
              (begin
                (display (substring text position (form-start (car remaining))) out)
                (display (car texts) out)
                (loop (form-end (car remaining)) (cdr remaining) (cdr texts))))))))))

(define (organize-imports-text text dialect)
  "Return @var{text} with the specs of every import form sorted.

Only a top-level import is organized: one written inside a `library' body
belongs to that body, and rewriting it would mean rewriting the library."
  (let* ((ast (ast-of-text text dialect))
         (top (filter node-data? ast))
         (forms (filter (lambda (form) (memq form top)) (import-forms ast))))
    (if (null? forms)
      text
      (call-with-output-string
        (lambda (out)
          (let loop ((position 0) (remaining forms))
            (if (null? remaining)
              (display (substring text position) out)
              (let* ((form (car remaining))
                     (start (form-start form))
                     (end (form-end form))
                     (specs (form-arguments form)))
                (display (substring text position start) out)
                (cond
                  ;; an import left with nothing in it is removed rather than
                  ;; written as `(import)', which no Scheme reads
                  [(null? specs) (loop (import-line-end text end) (cdr remaining))]
                  [else
                    (display (organize-import-form text form specs) out)
                    (loop end (cdr remaining))])))))))))

(define (unused-import-warnings-of config text dialect uri)
  "Return the unused-import findings of @var{text}, at every severity.

The action is asked for by hand, so it works from what the rule knows rather
than from what the project chose to display: a project that keeps
informational findings out of its problem list still wants its unused imports
gone when it says so."
  (filter (lambda (warning)
            (eq? (lint-warning-checker warning) 'unused-import))
          (lint-source text #:dialect dialect #:file (and uri (strip-uri uri))
                       #:checkers '(unused-import)
                       #:severity 'info
                       #:options (lint-options-of config))))

(define* (organize-imports-action config text #:key (dialect 'scheme) (uri #f)
                                  (version #f))
  "Return the action that removes unused imports and sorts the rest, or #f.

Two steps, both of them deletions and reorderings of what the author wrote:
the fixes `unused-import' already computes are applied first, then the specs
of every import form are put in order.  Nothing is renamed and nothing is
added, which is what makes this an action and not a guess."
  (let* ((warnings (unused-import-warnings-of config text dialect uri))
         (pruned (apply-fixes text warnings))
         (organized (organize-imports-text pruned dialect)))
    (and (not (string=? organized text))
      (json-object
       (field "title" "sift: organize imports")
       (field "kind" "source.organizeImports")
       (field "edit"
              (workspace-edit (or uri "") version text 0 (string-length text)
                              organized))))))

(define* (lsp-code-actions config text start end #:key (dialect 'scheme)
                           (uri #f) (version #f) (only '()))
  "Return the code actions that apply to @var{start}..@var{end} of @var{text}.

Three answers, in the order an editor should show them: the fixes for the
findings under the cursor, the one rewrite sift can make without being told
what to call anything, and the source actions -- fix everything, format the
definition the cursor is in, and organize the imports.  @var{only} is the
request's `context.only', so a client that asked for `source' gets the source
actions and nothing else."
  (let* ((warnings (warnings-for config text #:dialect dialect #:uri uri))
         (quickfixes
          (filter-map
           (lambda (warning)
             (and (lint-warning-fix warning)
               (lint-warning-range warning)
               (code-action-of warning text (or uri "") version)))
           (select-warnings warnings start end text)))
         (rewrite
          (and (only-accepts? only "refactor.rewrite")
            (format-form-action config text (ast-of-text text dialect) start
                                #:dialect dialect #:uri uri #:version version)))
         (fix-all
          (and (only-accepts? only "source.fixAll")
            (fix-all-action config text #:dialect dialect #:uri uri
                            #:version version)))
         (organize
          (and (only-accepts? only "source.organizeImports")
            (organize-imports-action config text #:dialect dialect #:uri uri
                                     #:version version))))
    (list->vector
     (append
      (if (only-accepts? only "quickfix") quickfixes '())
      (if rewrite (list rewrite) '())
      (if organize (list organize) '())
      (if fix-all (list fix-all) '())))))

(define (action-only params)
  "Return the action kinds a `codeAction' request asked for, as strings."
  (let ((only (json-ref (json-ref params "context" (json-object)) "only" #f)))
    (if (vector? only)
      (filter string? (vector->list only))
      '())))

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
                 #:blank-lines (config-ref config 'blank-lines)
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
           ;; Synchronisation is named field by field rather than as the bare
           ;; number 1, because the bare number says only how a change is
           ;; spelled.  A client reads a missing `save' as "this server does
           ;; not want saves", and `didSave' is how sift hears that the file
           ;; on disk is the buffer -- without it the index it answers jumps
           ;; from is never refreshed, and a definition added a minute ago is
           ;; still a definition sift cannot find.  1 under `change' is still
           ;; "full": the editor sends the whole buffer after every change.
           (field "textDocumentSync"
                  (json-object
                   (field "openClose" #t)
                   (field "change" 1)
                   (field "save" (json-object (field "includeText" #f)))))
           (field "documentFormattingProvider" #t)
           (field "codeActionProvider"
                  (json-object
                   (field "codeActionKinds" (list->vector code-action-kinds))
                   (field "resolveProvider" #f)))
           (field "hoverProvider" #t)
           (field "definitionProvider" #t)
           (field "referencesProvider" #t)
           (field "documentHighlightProvider" #t)
           (field "documentSymbolProvider" #t)
           (field "workspaceSymbolProvider" #t)
           ;; A hint is a parameter name beside its argument, so the editor
           ;; asks for the ones it is showing and sift answers about those:
           ;; there is nothing to fill in later, which is what
           ;; `resolveProvider' would promise.
           (field "inlayHintProvider"
                  (json-object (field "resolveProvider" #f)))
           (field "renameProvider"
                  (json-object (field "prepareProvider" #t)))
           (field "completionProvider"
                  (json-object (field "resolveProvider" #f)
                               (field "triggerCharacters"
                                      (list->vector (list "#" ":")))))
           ;; A signature is asked for while a call is being written, so the
           ;; characters that end an argument are the ones that ask for it.
           (field "signatureHelpProvider"
                  (json-object (field "triggerCharacters"
                                      (list->vector (list " " "(")))
                               (field "retriggerCharacters"
                                      (list->vector (list " ")))))
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
  (shutdown? session-shutdown? set-session-shutdown!)
  (watchers? session-watchers? set-session-watchers!)
  (registered? session-registered? set-session-registered!)
  ;; the configuration every finding is computed from, and the thunk that
  ;; reads it again: a project that edits its own `sift.yaml' in the editor it
  ;; is linting from should not have to restart the server to be heard
  (config session-config set-session-config!)
  (reload session-reload set-session-reload!)
  ;; the settings the editor itself sent, as JSON, kept so that reading the
  ;; project's files again does not drop them
  (settings session-settings set-session-settings!))

(define* (new-session #:key config reload)
  (let ((session (make-session)))
    (set-session-started! session #f)
    (set-session-shutdown! session #f)
    ;; Whether the client said it would register a watcher, and whether sift
    ;; has asked for one: asking a client that never promised is a request it
    ;; answers with an error sift would then have to ignore twice.
    (set-session-watchers! session #f)
    (set-session-registered! session #f)
    (set-session-config! session config)
    (set-session-reload! session reload)
    (set-session-settings! session #f)
    session))

(define (client-supports-watchers? params)
  "Return @code{#t} when the client advertises dynamic watcher registration."
  (equal? 'true
          (json-ref (json-ref (json-ref (json-ref params "capabilities")
                                        "workspace")
                              "didChangeWatchedFiles")
                    "dynamicRegistration")))

(define (config-files-patterns)
  "Return a watcher glob for each name a project's configuration may have.

A settings file is not a program sift indexes, and a client that watches only
`**/*.scm' would never say that `sift.yaml' was saved.  The configuration is
part of what a finding means -- the line length, the severity floor, which
rules run -- so a change to it is a change to every finding in the project."
  (map (lambda (name) (string-append "**/" name)) config-file-names))

(define (watcher-globs)
  "Return the glob patterns sift asks a client to watch."
  (append (map (lambda (extension) (string-append "**/*" extension))
               lisp-extensions)
          (config-files-patterns)))

(define (register-watchers! out session)
  "Ask the client to watch the files sift indexes, once per session."
  (unless (session-registered? session)
    (set-session-registered! session #t)
    (send! out
           (json-object
            (field "jsonrpc" "2.0")
            (field "id" "sift-register-watchers")
            (field "method" "client/registerCapability")
            (field "params"
                   (json-object
                    (field "registrations"
                           (vector
                            (json-object
                             (field "id" "sift-watch-lisp-files")
                             (field "method" "workspace/didChangeWatchedFiles")
                             (field "registerOptions"
                                    (json-object
                                     (field "watchers"
                                            (list->vector
                                             (map (lambda (glob)
                                                    (json-object
                                                     (field "globPattern" glob)))
                                                  (watcher-globs)))))))))))))))

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

(define (log-message! out type message)
  "Write @var{message} to the editor's log, at the protocol's @var{type}.

`window/showMessage' is for the person in front of the editor; this is for the
log they open when they want to know what the server did -- Eglot's *EGLOT
events* buffer is where a `window/logMessage' lands.  A server that says
nothing about its own work leaves a user with no way to tell \"it has nothing
to report\" from \"it is not running\", which is the question a quiet linter
raises most often.

@var{type} is 1 for an error, 2 a warning, 3 information and 4 a plain log
line; everything sift has to say about a step it took is information."
  (send! out
         (json-object
          (field "jsonrpc" "2.0")
          (field "method" "window/logMessage")
          (field "params"
                 (json-object
                  (field "type" type)
                  (field "message" message))))))

(define (notice! out message)
  "Log an information-level @var{message} in the editor."
  (log-message! out 3 message))

(define (document-name document)
  "Return the short name of @var{document}, for a log line."
  (let ((path (strip-uri (document-uri document))))
    (let ((slash (string-rindex path #\/)))
      (if slash (substring path (+ slash 1)) path))))

(define (findings-sentence diagnostics)
  "Return what a log line says about @var{diagnostics}."
  (let ((count (vector-length diagnostics)))
    (case count
      [(0) "no findings"]
      [(1) "1 finding"]
      [else (format #f "~a findings" count)])))

(define (publish-diagnostics! out uri version diagnostics)
  "Tell the editor what @var{diagnostics} the document at @var{uri} has.

The version is written only when it is one: `json-write' turns @code{#f} into
JSON `false', and a client comparing `false' against the version of its buffer
either refuses the report or errors on the comparison.  A report with no
version is one a client applies to whatever it has, which is the right answer
for a document whose version sift was never told."
  (send! out
         (json-object
          (field "jsonrpc" "2.0")
          (field "method" "textDocument/publishDiagnostics")
          (field "params"
                 (apply json-object
                        (append
                         (list (field "uri" uri))
                         (if (number? version)
                           (list (field "version" version))
                           '())
                         (list (field "diagnostics" diagnostics))))))))

(define* (publish! config out document #:key (force #f))
  "Send the diagnostics of @var{document} to the editor, unless nothing changed.

A buffer that does not parse is a buffer with findings -- the reader's own,
with severity `error` and the code `reader` -- so this works the same way
whether the text is a program or half of one.

What is remembered is the text the last report was about, not the findings it
carried.  \"The findings are the same as last time\" is not the same question
as \"the editor already has this report\": an editor drops a
`publishDiagnostics' whose version is not the version of the buffer it is
showing (Eglot returns early from `eglot--flymake-handle-push' when the two
disagree), so a report sift withheld because it looked familiar can be a
report the editor never took, and the squiggle it described stays under a line
that no longer says that.  The text, on the other hand, is exactly what the
editor and the server can agree on: the same text has the same findings, and a
report about text the editor already has is the one report worth not sending
again.

@var{force} says the findings may differ even though the text did not -- the
project it was read against changed -- which is what a save or a configuration
reload asks for."
  (let* ((text (document-text document))
         (diagnostics (lsp-diagnostics config
                                       text
                                       #:dialect (document-dialect document)
                                       #:uri (document-uri document))))
    (unless (and (not force)
              (document-published document)
              (string=? (cdr (document-published document)) text))
      (set-document-published! document (cons diagnostics text))
      (notice! out
               (format #f "sift: ~a in ~a"
                       (findings-sentence diagnostics) (document-name document)))
      (publish-diagnostics! out
                            (document-uri document)
                            (document-version document)
                            diagnostics))))

(define* (publish-all! config out documents #:key (force #f))
  "Send the findings of every open document to the editor.

Used where what changed is the project rather than a buffer -- a save, a file
changed on disk, a configuration reload.  A finding can be about a file the
editor is not looking at (`unused-definition', a library that is not
installed), so the buffer whose text is unchanged is exactly the buffer that
has to be asked again."
  (hash-for-each (lambda (uri document)
                   (publish! config out document #:force force))
                 documents))

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

;;; --- the project's configuration -----------------------------------------
;;;
;;; Every finding is a function of the text and of the settings it was read
;;; against, and the settings come from files the reader may be editing in
;;; this very session.  So the configuration is a value the session holds and
;;; can replace, never a constant a server was started with.

(define (config-path? path)
  "Return @code{#t} when @var{path} is one of the names a project's settings
may have."
  (let ((name (let ((slash (string-rindex path #\/)))
                (if slash (substring path (+ slash 1)) path))))
    (and (member name config-file-names) #t)))

(define (forget-workspace! workspace)
  "Forget everything the index knows, so the next question reads the project.

A configuration decides which files are sift's to read -- `file-filter' -- so
a session that kept its index through a settings change could be answering
about a file the project has just excluded, or missing one it has just
included."
  (hash-clear! (workspace-files workspace))
  (set-workspace-scanned! workspace #f))

;;; --- the editor's own settings -------------------------------------------
;;;
;;; An editor can configure a language server, and it does it in two places:
;;; `initializationOptions' when the session starts, and
;;; `workspace/didChangeConfiguration' whenever the user's settings change --
;;; which is Eglot's `eglot-workspace-configuration', and the same object a VS
;;; Code client keeps in its `settings.json'.  Both arrive as one JSON object,
;;; and sift reads it as one more layer of its own configuration: above the
;;; files, because what the editor says is the most specific thing anyone can
;;; say about this session.  A key sift does not know is a key it does not
;;; pretend to have read -- it is logged, and the layer below wins.

(define (settings-object-of params key)
  "Return the sift section of the settings a client sent under @var{key}.

Eglot sends `eglot-workspace-configuration', which is organized by server:
`(:sift (:inlay-hints :json-false))' arrives as `{\"sift\": {...}}'.  A client
that keeps one flat object of settings is understood too, because the object
is only nested when there is a section to nest."
  (let* ((object (json-ref params key #f))
         (section (and (json-object? object) (json-ref object "sift" #f))))
    (cond
      [(json-object? section) section]
      [(json-object? object) object]
      [else #f])))

(define (json-value->setting value)
  "Return a JSON value as the value a configuration key is written with.

JSON's booleans arrive as symbols and a list arrives as a vector, so this is
the one place where the two spellings meet.  An object is not a setting sift
has: no key takes one."
  (cond
    [(eq? value json-true) #t]
    [(eq? value json-false) #f]
    [(vector? value) (map json-value->setting (vector->list value))]
    [(json-object? value) #f]
    [else value]))

(define (setting-with-complaint config entry out)
  "Return @var{config} with one client @var{entry} applied, or unchanged.

Every value is checked by the same code that checks a `sift.yaml' entry, so a
project cannot set from the editor what it could not set from its own file,
and a value of the wrong shape is reported and skipped rather than believed."
  (let* ((name (car entry))
         (key (string->symbol name))
         (value (json-value->setting (cdr entry))))
    (if (not (config-key-documented? key))
      (begin
        (log-message! out 2
                      (format #f "sift: no setting is called `~a'; ignoring it" name))
        config)
      (let ((result (catch 'sift-config-error
                           (lambda ()
                             (config-set config key (validate-setting key value)
                                         "the editor's settings"))
                           (lambda (kind object) object))))
        (if (config? result)
          result
          (begin
            (log-message! out 2
                          (format #f "sift: ~a" (config-error->string result)))
            config))))))

(define (apply-client-settings config settings out)
  "Return @var{config} with the client's @var{settings} applied over it."
  (fold (lambda (entry config) (setting-with-complaint config entry out))
        config
        (json-fields settings)))

(define (settings-config session out config)
  "Return @var{config} with the settings the client last sent applied over it.

The client's settings are kept on the session rather than folded into the
configuration once, because re-reading `sift.yaml' -- a save, a reload -- must
not quietly drop what the editor asked for."
  (let ((settings (session-settings session)))
    (if settings (apply-client-settings config settings out) config)))

(define (reload-config! session workspace out documents)
  "Read the configuration again, and check the open buffers against it.

A reload that cannot be had -- the file was saved mid-edit and does not parse,
or there is no reloader because the server was started without one -- is not
a reason to change what the session already knows: the previous configuration
is the last one that worked, and a linter that forgot its settings because of
one bad save would go quiet at the worst moment."
  (let* ((reload (session-reload session))
         (config (and reload (reload))))
    (cond
      [config
        (set-session-config! session (settings-config session out config))
        (set-workspace-filter!
         workspace (config-ref (session-config session) 'file-filter))
        (forget-workspace! workspace)
        (let ((open (hash-count (const #t) documents)))
          (notice! out
                   (format #f "sift: re-read the configuration; ~a open buffer~a to check"
                           open (if (= 1 open) "" "s"))))
        ;; The text did not change but the yardstick did, so what sift last
        ;; said about that text is not what it would say now.
        (publish-all! (session-config session) out documents #:force #t)]
      [reload
        (notice! out (string-append
                      "sift: the configuration could not be read; "
                      "keeping the one the session started with"))
        ;; The files may still have changed in a way that matters even when
        ;; they could not be understood -- a save is a save.
        (publish-all! (session-config session) out documents #:force #t)]
      [else
        ;; No reloader: the caller of `lsp-serve' owns the configuration, so
        ;; checking the buffers again is all that can be said about a change.
        (notice! out
                 (string-append "sift: this session cannot re-read its"
                                " configuration; checking the open buffers"))
        (publish-all! (session-config session) out documents #:force #t)])))

(define (refresh-changed-files! workspace changes)
  "Re-read the files @var{changes} names, from disk.

Two values: how many files were read again, and whether one of them was a
configuration file -- which is not a program sift indexes, but the yardstick
every finding is measured with, so a change to one is a change to all of
them."
  (let loop ((rest changes) (refreshed 0) (settings? #f))
    (if (null? rest)
      (values refreshed settings?)
      (let ((uri (json-ref (car rest) "uri" #f)))
        (cond
          [(not (and (string? uri) (string-prefix? "file://" uri)))
           (loop (cdr rest) refreshed settings?)]
          [(config-path? (strip-uri uri))
           (loop (cdr rest) refreshed #t)]
          ;; A project sift has not read yet needs no refreshing: whatever it
          ;; is asked next reads the disk as it is at that moment.
          [(not (workspace-scanned? workspace)) (loop (cdr rest) refreshed settings?)]
          [else
            (workspace-refresh! workspace (strip-uri uri))
            (loop (cdr rest) (+ refreshed 1) settings?)])))))

;;; --- the handlers -------------------------------------------------------

(define* (handle session workspace documents message out)
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
  ;; The configuration lives on the session rather than in this call, because
  ;; it is the one thing a message can replace: `workspace/didChangeConfiguration'
  ;; says the project's settings are not the ones the session started with.
  (let ((config (session-config session))
        (method (json-ref message "method" ""))
        (id (json-ref message "id")))
    (cond
      [(string=? method "initialize")
       (set-session-started! session #t)
       (set-session-watchers! session (client-supports-watchers? (params)))
       ;; The root is where a question about \"the project\" is answered from;
       ;; an editor that names none still gets its open buffers searched.
       (set-workspace-root! workspace (workspace-root-of (params)))
       ;; Settings an editor sends with `initialize' are the first layer
       ;; above the files, and they stay until it sends others.
       (set-session-settings! session
                              (settings-object-of (params) "initializationOptions"))
       (set-session-config! session
                            (settings-config session out (session-config session)))
       (reply! out id (initialize-result))
       (notice! out
                (format #f "sift ~a: serving ~a"
                        sift-version
                        (or (workspace-root workspace) "the open buffers")))
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
       ;; The watcher is registered after the client says it is ready: a
       ;; request sent before that is a request the client may drop.
       (if (session-watchers? session)
         (begin
           (register-watchers! out session)
           (notice! out
                    "sift: watching the project's Lisp files and its configuration"))
         (notice! out (string-append
                       "sift: this client does not watch files; "
                       "a save is the only news I get about the project")))
       'continue]

      [(and (request?) (not (json-ref message "method" #f)))
       ;; An answer to a request sift made -- registering the watcher -- which
       ;; the protocol requires to arrive and which there is nothing to do
       ;; with: the watcher either was registered or the changes will not come.
       'continue]

      [(string=? method "shutdown")
       (set-session-shutdown! session #t)
       (reply! out id json-null)
       'continue]

      [(string=? method "textDocument/didOpen")
       (let* ((item (json-ref (params) "textDocument"))
              (uri (json-ref item "uri" ""))
              (language (json-ref item "languageId" #f))
              (document (new-document uri
                                      (json-ref item "text" "")
                                      language
                                      (client-version (json-ref item "version" #f))
                                      (dialect-of language uri))))
         (hash-set! documents uri document)
         (notice! out
                  (format #f "sift: opened ~a as ~a"
                          (document-name document) (document-dialect document)))
         (publish! config out document)
         'continue)]

      [(string=? method "textDocument/didChange")
       (let ((document (document-of)))
         (when document
           (set-document-text! document
                               (apply-changes (document-text document)
                                              (json-ref (params) "contentChanges" (vector))))
           (set-document-version!
            document
            (client-version (json-ref (json-ref (params) "textDocument") "version" #f)))
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
       ;; A save is the editor telling sift the file on disk is this buffer:
       ;; the copy in the index is now out of date, and so is every finding
       ;; that was computed against it -- in this buffer and in the others,
       ;; because a finding can name a definition that lives in a file the
       ;; reader is not looking at.
       (let ((document (document-of)))
         (when document
           (let ((uri (document-uri document)))
             (when (and (workspace-scanned? workspace)
                     (string-prefix? "file://" uri))
               (workspace-refresh! workspace (strip-uri uri))
               (notice! out (format #f "sift: re-read ~a after a save"
                                    (document-name document)))))
           (publish-all! config out documents #:force #t)))
       'continue]

      [(string=? method "workspace/didChangeWatchedFiles")
       ;; A file changed, appeared or went away without the editor opening it:
       ;; the watcher sift asked for is the reason it hears about it.
       (let ((changes (json-ref (params) "changes" (vector))))
         (when (vector? changes)
           (call-with-values
             (lambda () (refresh-changed-files! workspace (vector->list changes)))
             (lambda (refreshed settings?)
               (when (positive? refreshed)
                 (notice! out
                          (format #f "sift: ~a file~a changed on disk"
                                  refreshed (if (= refreshed 1) "" "s"))))
               ;; A configuration file is not a program: what changed is what
               ;; a finding means, so the settings are read again rather than
               ;; only the file.
               (cond
                 [settings? (reload-config! session workspace out documents)]
                 [(positive? refreshed)
                  (publish-all! config out documents #:force #t)])))))
       'continue]

      [(string=? method "workspace/didChangeConfiguration")
       ;; A client that says the settings changed is a client whose next
       ;; finding can be a different finding.  Its own settings come with the
       ;; message -- Eglot sends the whole of `eglot-workspace-configuration'
       ;; every time -- and the project's files are read again under them,
       ;; because a setting can change what a finding is measured against.
       (set-session-settings! session
                              (settings-object-of (params) "settings"))
       (reload-config! session workspace out documents)
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
                                       #:version (document-version document)
                                       #:only (action-only (params))))
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
                 (if (and document (config-ref config 'hover))
                   (let* ((text (document-text document))
                          (dialect (document-dialect document))
                          (position (json-ref (params) "position" (json-object)))
                          (index (index-of-text-position text position))
                          ;; The definition may be in another file, so the hover
                          ;; asks the same question a jump does and describes
                          ;; the definition it finds there.
                          (sites (definition-sites workspace documents
                                                   (document-uri document)
                                                   text dialect index))
                          (site (and (pair? sites) (car sites))))
                     ;; A symbol is the question an editor's hover usually asks;
                     ;; the rule under the cursor is the one sift alone can
                     ;; answer, so it is the fallback rather than the first
                     ;; answer.
                     (or (and site (symbol-hover (cadr site) dialect (caddr site)))
                       (lsp-hover config text index
                                  #:dialect dialect
                                  #:uri (document-uri document))
                       json-null))
                   json-null))
         'continue)]

      [(string=? method "textDocument/definition")
       (let ((document (document-of)))
         (reply! out id
                 (if document
                   (lsp-definition workspace documents
                                   (document-uri document)
                                   (document-text document)
                                   (document-dialect document)
                                   (index-of-text-position
                                    (document-text document)
                                    (json-ref (params) "position" (json-object))))
                   (vector)))
         'continue)]

      [(string=? method "textDocument/references")
       (let ((document (document-of)))
         (reply! out id
                 (if document
                   (lsp-references workspace documents
                                   (document-uri document)
                                   (document-text document)
                                   (document-dialect document)
                                   (index-of-text-position
                                    (document-text document)
                                    (json-ref (params) "position" (json-object)))
                                   #:include-declaration
                                   (json-ref (json-ref (params) "context" (json-object))
                                             "includeDeclaration" #t))
                   (vector)))
         'continue)]

      [(string=? method "textDocument/documentHighlight")
       (let ((document (document-of)))
         (reply! out id
                 (if document
                   (lsp-document-highlights
                    (document-text document)
                    (document-dialect document)
                    (index-of-text-position
                     (document-text document)
                     (json-ref (params) "position" (json-object))))
                   (vector)))
         'continue)]

      [(string=? method "textDocument/documentSymbol")
       (let ((document (document-of)))
         (reply! out id
                 (if document
                   (lsp-document-symbols (document-text document)
                                         #:dialect (document-dialect document))
                   (vector)))
         'continue)]

      [(string=? method "workspace/symbol")
       ;; No buffer is named by this request, so the search is over every
       ;; dialect: the editor has already decided what it is looking for.
       (reply! out id
               (workspace-symbols workspace documents
                                  (json-ref (params) "query" "")
                                  #f))
       'continue]

      [(string=? method "textDocument/completion")
       (let ((document (document-of)))
         (reply! out id
                 (if (and document (config-ref config 'completion))
                   (completion-items workspace documents
                                     (document-uri document)
                                     (document-text document)
                                     (document-dialect document)
                                     (index-of-text-position
                                      (document-text document)
                                      (json-ref (params) "position" (json-object))))
                   (vector)))
         'continue)]

      [(string=? method "textDocument/inlayHint")
       ;; The editor asks for the hints of the part of the file it is
       ;; showing, so the answer is about that range and nothing else.
       (let ((document (document-of)))
         (if (not (and document (config-ref config 'inlay-hints)))
           (reply! out id (vector))
           (let* ((text (document-text document))
                  (span (range-of-request text (params))))
             (reply! out id
                     (lsp-inlay-hints workspace documents
                                      (document-uri document) text
                                      (document-dialect document)
                                      (car span) (cdr span))))))
       'continue]

      [(string=? method "textDocument/signatureHelp")
       (let ((document (document-of)))
         (reply! out id
                 (if document
                   (or (lsp-signature-help workspace documents
                                           (document-uri document)
                                           (document-text document)
                                           (document-dialect document)
                                           (index-of-text-position
                                            (document-text document)
                                            (json-ref (params) "position"
                                                      (json-object))))
                     json-null)
                   json-null))
         'continue)]

      [(string=? method "textDocument/prepareRename")
       (let ((document (document-of)))
         (reply! out id
                 (if document
                   (let* ((text (document-text document))
                          (dialect (document-dialect document))
                          (ast (ast-of-text text dialect))
                          (index (index-of-text-position
                                  text (json-ref (params) "position" (json-object))))
                          (name (resolve-name ast index))
                          (node (symbol-node-at ast index)))
                     (if (and name node)
                       (json-object
                        (field "range" (json-range text (form-start node)
                                                   (form-end node)))
                        (field "placeholder" name))
                       json-null))
                   json-null))
         'continue)]

      [(string=? method "textDocument/rename")
       (let ((document (document-of)))
         (if (not document)
           (reply! out id json-null)
           (let* ((text (document-text document))
                  (dialect (document-dialect document))
                  (index (index-of-text-position
                          text (json-ref (params) "position" (json-object))))
                  (name (resolve-name (ast-of-text text dialect) index))
                  (new-name (json-ref (params) "newName" "")))
             (cond
               [(not name) (reply! out id json-null)]
               [(not (valid-symbol-name? new-name))
                (reply-error! out id -32602
                              (format #f "sift: `~a' is not a name a symbol can have"
                                      new-name))]
               [else (reply! out id
                             (rename-edit workspace documents (document-uri document)
                                          text dialect index new-name
                                          #:version (document-version document)))])))
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

(define (serve-one session workspace documents message out)
  "Answer @var{message}, turning a raised condition into an error response."
  (let ((id (json-ref message "id" *no-id*)))
    (catch #t
           (lambda () (handle session workspace documents message out))
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

(define* (lsp-serve config #:key (in #f) (out #f) (reload #f))
  "Serve one editor until it says `exit'.

@var{in} and @var{out} are the binary ports of the conversation, standard
input and standard output by default.

@var{reload} is a thunk that returns the configuration as the project's files
now describe it, or @code{#f} when they cannot be read.  It is called when the
editor says the settings changed and when a watched configuration file is
saved; a server started without one keeps the configuration it was given and
says so rather than pretending to have re-read it.

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
        (workspace (new-workspace config))
        (session (new-session #:config config #:reload reload)))
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
            (let ((outcome (serve-one session workspace documents
                                      (cdr frame) port-out)))
              (if (eq? outcome 'exit)
                (if (session-shutdown? session) 0 1)
                (loop status)))])))))
