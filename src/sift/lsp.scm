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
;;   workspace/willRenameFiles         rename the module a file names, and its imports
;;   workspace/didRenameFiles          re-read the renamed file
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
;; rest of sift: `inlay-hints' (which kinds of hint, or none), `hover' and
;; `completion' are settings like `line-length' is, so a project can set them
;; in its `sift.yaml'.  They can also be set by the editor itself --
;; `initializationOptions' when the session starts, and
;; `eglot-workspace-configuration' or a `settings.json' through
;; `workspace/didChangeConfiguration' -- and those settings are read by the
;; same validator and layered over the files, because what the editor says is
;; the most specific thing anyone can say about this session.
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
  #:use-module (ice-9 threads)
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
  #:use-module (sift analysis)
  #:use-module (sift stdlib)
  #:use-module (sift compiler)
  #:export (lsp-serve
            lsp-diagnostics
            lsp-code-actions
            lsp-formatting
            organize-imports-text)
  ;; The questions about a buffer are answered by `(sift analysis)', and they
  ;; are exported here as they always were: a caller of `(sift lsp)' should not
  ;; have to know which half of the server moved.
  #:re-export (lsp-definition
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

(define *send-lock* (make-mutex))

(define* (send! out message)
  "Write @var{message} to @var{out} as one framed JSON-RPC message.

The lock is what makes a report from the second pass and a report from the
message loop one after the other rather than one inside the other: two frames
interleaved on the wire are a protocol error, and an editor reads that as a
dead server rather than as a finding." ;; one bytevector, one write: a header and a body written separately are two
  ;; chances for another thread to put its frame between them, and a client
  ;; that reads 815 bytes of two messages is a client that closes the session
  (with-mutex *send-lock*
              (let* ((body (string->utf8 (json-write message)))
                     (header (string->utf8
                              (format #f "Content-Length: ~a\r\n\r\n"
                                      (bytevector-length body))))
                     (header-length (bytevector-length header))
                     (body-length (bytevector-length body))
                     (frame (make-bytevector (+ header-length body-length))))
                (bytevector-copy! header 0 frame 0 header-length)
                (bytevector-copy! body 0 frame header-length body-length)
                (put-bytevector out frame)
                (force-output out))))
;;; ===== The project on disk =====
;;;
;;; A project is at most a couple of thousand files and a few hundred kilobytes
;;; a file; past that an index costs more than the jumps it answers.

(define index-file-limit 2000)
(define index-byte-limit 500000)
;;;
;;; `(sift analysis)' answers about a workspace; this is the part that knows
;;; where one comes from.  The tree under the root is read once and cached
;;; beside the user's other caches, a file the editor saved is read again, and a
;;; file a watcher reports is read again -- and the two parameters at the end
;;; hand that knowledge to the analysis, which asks for it by name.

(define (directory? path)
  (eq? 'directory (false-if-exception (stat:type (stat path)))))

(define (indexable-file? filter path root)
  "Return @code{#t} when @var{path} is a file the server indexes.

Navigation reads a compressed source as the file it holds, so a `.el.gz' is
indexed when the project names no filter.  A project that names one keeps the
last word, and can name `*.el.gz' itself; the command line never takes this
path, because `sift format --write' must not write decompressed text over a
compressed file."
  (if (or (not filter) (null? filter))
    (lisp-path? (without-compression path))
    (file-included? filter path root)))

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
                   [(indexable-file? filter path root)
                    (set! files (cons path files))]))))
           entries)
          (loop (append subdirectories (cdr pending))
                (append files found)
                (+ count (length files))))])))

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
        (text (read-file-text path index-byte-limit)))
    (and dialect text
      (let ((definitions (parse-file-text text dialect path)))
        (make-indexed-file path text dialect definitions
                           (top-level-names definitions))))))
(define cache-version 1)

;; `cache-home' -- where a cache belongs -- is the standard library's, because
;; a cache directory is one directory: two definitions of it would be two
;; opinions about where a cache is, and they would drift.

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

(define (scan-workspace! workspace)
  "Fill @var{workspace}'s index from the tree its root names.

The scan reads the names of the files and nothing else: the text and the
definitions of a file arrive when a request needs them.  A file whose size and
modification time are the same as the last session's starts from the names the
cache remembers, and is parsed the moment something asks for more."
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
      (write-cache! root workspace))))

(define (workspace-refresh! workspace path)
  "Read @var{path} again, or forget it when it is gone or no longer wanted.

A file the editor saved, and a file a watcher reported, are the two ways a
project changes under a running server.  A file that is no longer one of the
project's -- deleted, or excluded by a filter the project has since changed --
is forgotten rather than kept as a stale answer."
  (let* ((root (workspace-root workspace))
         (wanted? (and root (file-exists? path)
                    (indexable-file? (workspace-filter workspace) path root))))
    (if wanted?
      (let ((entry (index-file path)))
        (if entry
          (hash-set! (workspace-files workspace) path entry)
          (hash-remove! (workspace-files workspace) path)))
      (hash-remove! (workspace-files workspace) path))))

(define (load-indexed-file! entry)
  "Read @var{entry}'s file, filling in the text and the definitions it lacks."
  (let ((path (indexed-file-path entry)))
    (let ((text (read-file-text path index-byte-limit)))
      (when text
        (set-indexed-file-text! entry text)
        (set-indexed-file-definitions! entry
                                       (parse-file-text text
                                                        (indexed-file-dialect entry)
                                                        path))))))

;; The two holes `(sift analysis)' leaves for a build that has a filesystem,
;; filled here, in the module that has one.
(current-workspace-scan scan-workspace!)
(current-index-loader load-indexed-file!)
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

;; The rules that need the whole file before they can say anything.  They are
;; the ones that would make a keystroke wait, so the language server computes
;; them in a thread of its own and publishes what they find as a second report.
(define deep-checkers
  '(call-arity string-literal-argument number-literal-argument
               eq-non-symbol unused-import-from))

(define* (all-warnings config text #:key (dialect 'scheme) (uri #f) (exclude '()))
  "Return every warning @var{text} earns, leaving out @var{exclude}."
  (with-lint-lock
   (lambda ()
     (lint-source text #:dialect dialect #:file (and uri (strip-uri uri))
                  #:checkers (config-ref config 'checkers)
                  #:exclude-checkers (append exclude
                                             (or (config-ref config 'exclude-checkers)
                                               '()))
                  #:severity (config-ref config 'severity)
                  #:options (lint-options-of config)))))

(define* (warnings-for config text #:key (dialect 'scheme) (uri #f))
  "Return the warnings @var{text} earns, as the editor should see them.

The whole-file rules are not among them: the report they belong to is the
second one, and this is the one an editor gets while the reader is still
typing."
  (all-warnings config text #:dialect dialect #:uri uri
                #:exclude deep-checkers))

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

;;; ===== Renaming a file, and the module it names =====
;;;
;;; A file is not a module in any of the three dialects, but it is the thing a
;;; reader renames when the module changes: the editor asks before the move,
;;; and the answer is the edits that keep the name and the file in step.  The
;;; three dialects disagree about what a module is, so each has its own rule:
;;; Scheme names a library in a form and imports it by that name; Emacs Lisp
;;; names a feature with `provide' and asks for it with `require'; Common Lisp
;;; has packages, and a package is not a file, so a file rename changes no
;;; name there and the answer is nothing.

(define (path-basename path)
  "Return the last segment of @var{path}."
  (let ((slash (string-rindex path #\/)))
    (if slash (substring path (+ slash 1)) path)))

(define (path-base path)
  "Return the file name @var{path} ends in, without compression or extension."
  (let ((name (path-basename path)))
    (let ((name (if (string-suffix? ".gz" name)
                  (string-drop-right name 3)
                  name)))
      (let ((dot (string-rindex name #\.)))
        (if dot (substring name 0 dot) name)))))

(define (module-part text)
  "Return @var{text} as a Scheme module segment: underscores become hyphens."
  (string-map (lambda (character)
                (if (char=? character #\_) #\- character))
              text))

(define (last-atomic-element node)
  "Return the last atom of a list @var{node}, or @code{#f}."
  (let ((atoms (filter node-atom? (data-elements node))))
    (and (pair? atoms) (last atoms))))

(define (project-file-texts workspace documents old-uri new-uri dialect)
  "Return @code{(uri text)} for the project's files in @var{dialect}.

The file being renamed is left out: its edit is written against the name the
editor is moving it to, and the old name is about to stop existing."
  (let ((sites '()))
    (hash-for-each
     (lambda (uri document)
       (when (and (not (string=? uri old-uri))
               (not (string=? uri new-uri))
               (eq? (document-dialect document) dialect))
         (set! sites (cons (list uri (document-text document)) sites))))
     documents)
    (hash-for-each
     (lambda (path entry)
       (let ((uri (path->uri path)))
         (when (and (not (string=? uri old-uri))
                 (not (string=? uri new-uri))
                 (eq? (indexed-file-dialect entry) dialect))
           (set! sites (cons (list uri (indexed-file-text-of entry)) sites)))))
     (workspace-index workspace))
    (reverse sites)))

(define (scheme-library-form ast)
  "Return the first library form of @var{ast}, or @code{#f}."
  (let ((forms (library-forms ast)))
    (and (pair? forms) (car forms))))

(define (scheme-rename-edits old-path new-path new-uri old-text new-text sites)
  "Return the edits that rename the module in @var{old-text} with the file."
  (let* ((ast (ast-of-text old-text 'scheme))
         (form (scheme-library-form ast))
         (old-name (and form (library-name form)))
         (name-node (and form (cadr (data-elements form))))
         (name-atom (and name-node (last-atomic-element name-node)))
         (old-base (path-base old-path))
         (new-base (module-part (path-base new-path))))
    (if (and (pair? old-name) name-atom
          (string=? (car (reverse old-name)) old-base)
          (not (string=? old-base new-base)))
      (cons (list new-uri new-text (form-start name-atom) (form-end name-atom)
                  new-base)
            (append-map
             (lambda (site)
               (let* ((uri (car site))
                      (text (cadr site))
                      (imports (import-sites (ast-of-text text 'scheme))))
                 (append-map
                  (lambda (spec)
                    (if (equal? (spec-library spec) old-name)
                      (let ((atom (last-atomic-element (spec-library-node spec))))
                        (if atom
                          (list (list uri text (form-start atom) (form-end atom)
                                      new-base))
                          '()))
                      '()))
                  (append-map import-site-specs imports))))
             sites))
      '())))

(define (quoted-atom node)
  "Return the atom a quote prefix or @code{(quote ...)} holds."
  (cond
    [(and node (node-atom? node)) node]
    [(and node (node-prefix? node) (node-atom? (node-child node)))
     (node-child node)]
    [(and node (node-list? node) (eq? (node-head-name node) 'quote))
     (let ((argument (cadr (data-elements node))))
       (and argument (node-atom? argument) argument))]
    [else #f]))

(define (forms-headed-by ast head)
  "Return every form in @var{ast} whose head names @var{head}, in source order."
  (let ((found '()))
    (define (walk node)
      (cond
        [(node-list? node)
         (when (equal? (node-head-name node) head)
           (set! found (cons node found)))
         (for-each (lambda (child)
                     (when (node-data? child) (walk child)))
                   (node-elements node))]
        [(node-prefix? node) #f]
        [else #f]))
    (for-each (lambda (node) (when (node-data? node) (walk node))) ast)
    (reverse found)))

(define (provided-feature ast)
  "Return @code{(node . name)} for the first @code{provide} form of @var{ast}."
  (let* ((form (car (forms-headed-by ast 'provide)))
         (atom (and form (quoted-atom (cadr (data-elements form))))))
    (and atom (cons atom (node-value atom)))))

(define (elisp-rename-edits old-path new-path new-uri old-text new-text sites)
  "Return the edits that rename the feature in @var{old-text} with the file."
  (let* ((ast (ast-of-text old-text 'elisp))
         (provided (provided-feature ast))
         (old-feature (or (and provided (cdr provided)) (path-base old-path)))
         (new-feature (path-base new-path)))
    (if (string=? old-feature new-feature)
      '()
      (append
       (if provided
         (let ((atom (car provided)))
           (list (list new-uri new-text
                       (source-location-index (node-start atom))
                       (source-location-index (node-end atom))
                       new-feature)))
         '())
       (append-map
        (lambda (site)
          (let* ((uri (car site))
                 (text (cadr site))
                 (requires (forms-headed-by (ast-of-text text 'elisp) 'require)))
            (append-map
             (lambda (form)
               (let ((atom (quoted-atom (cadr (data-elements form)))))
                 (if (and atom (string=? (node-value atom) old-feature))
                   (list (list uri text
                               (source-location-index (node-start atom))
                               (source-location-index (node-end atom))
                               new-feature))
                   '())))
             requires)))
        sites)))))

(define (file-rename-edits workspace documents old-uri new-uri)
  "Return the edits that keep the module a renamed file names in step."
  (let* ((old-path (strip-uri old-uri))
         (new-path (strip-uri new-uri))
         (document (hash-ref documents old-uri #f))
         (old-text (or (and document (document-text document))
                     (read-file-text old-path index-byte-limit)))
         (dialect (or (and document (document-dialect document))
                    (detect-dialect #:file old-path)
                    (detect-dialect #:file new-path))))
    (if (not (and old-text dialect))
      '()
      (let ((sites (project-file-texts workspace documents old-uri new-uri
                                       dialect)))
        (case dialect
          [(scheme) (scheme-rename-edits old-path new-path new-uri old-text old-text
                                         sites)]
          [(elisp) (elisp-rename-edits old-path new-path new-uri old-text old-text
                                       sites)]
          [else '()])))))

(define (workspace-edit-of edits)
  "Return a @code{WorkspaceEdit} from @code{(uri text start end replacement)}."
  (let ((by-uri (make-hash-table)))
    (for-each
     (lambda (edit)
       (hash-set! by-uri (car edit)
                  (cons edit (hash-ref by-uri (car edit) '()))))
     edits)
    (json-object
     (field "documentChanges"
            (list->vector
             (hash-map->list
              (lambda (uri entries)
                (json-object
                 (field "textDocument" (text-document-of uri #f))
                 (field "edits"
                        (list->vector
                         (map (lambda (edit)
                                (let ((text (cadr edit))
                                      (start (caddr edit))
                                      (end (cadddr edit))
                                      (replacement (car (cddddr edit))))
                                  (json-object
                                   (field "range" (json-range text start end))
                                   (field "newText" replacement))))
                              (reverse entries))))))
              by-uri))))))

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
  ;; offers an editor: two kinds of finding fix, the two rewrites that need no
  ;; name but the one already in the source, the one rewrite that does, and
  ;; the source actions -- fix everything, the two import operations, and the
  ;; standard "organize imports" that does both.
  '("quickfix" "refactor.extract" "refactor.inline" "refactor.rewrite"
    "source.fixAll" "source.fixAll.sift" "source.organizeImports"
    "source.optimizeImports.sift" "source.sortImports.sift"))
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

(define (replace-spans text edits)
  "Return @var{text} with each @code{(start end replacement)} applied."
  (if (null? edits)
    text
    (call-with-output-string
      (lambda (out)
        (let loop ((position 0)
                   (remaining (sort edits (lambda (a b) (< (car a) (car b))))))
          (if (null? remaining)
            (display (substring text position) out)
            (let ((edit (car remaining)))
              (display (substring text position (car edit)) out)
              (display (caddr edit) out)
              (loop (cadr edit) (cdr remaining)))))))))

(define (module-import-clauses form)
  "Return the @code{#:use-module} clauses of a @code{define-module} form."
  (filter (lambda (clause)
            (let ((keyword (car clause)))
              (and (node-atom? keyword)
                (equal? (node-value keyword) "#:use-module"))))
          (declaration-clauses form)))

(define (module-import-edits text form)
  "Return the edits that sort the @code{#:use-module} clauses of @var{form}."
  (let* ((clauses (module-import-clauses form))
         (sorted (stable-sort
                  (list-copy clauses)
                  (lambda (a b)
                    (string<? (import-spec-key (cdr a))
                              (import-spec-key (cdr b)))))))
    (map (lambda (clause replacement)
           (list (form-start (car clause)) (form-end (cdr clause)) replacement))
         clauses
         (map (lambda (clause)
                (substring text (form-start (car clause)) (form-end (cdr clause))))
              sorted))))

(define (organize-imports-text text dialect)
  "Return @var{text} with every import form's specs sorted.

Both shapes a Scheme file uses are read: a top-level @code{(import ...)} form
-- wherever it is written, including inside a `library' body -- and the
@code{#:use-module} clauses of a @code{define-module}.  The specifications are
permuted in place, so a form written on one line stays on one line and a form
written one spec per line keeps its indentation."
  (let* ((ast (ast-of-text text dialect))
         (top (filter node-data? ast))
         (forms (filter (lambda (form) (memq form top)) (import-forms ast)))
         (modules (filter (lambda (form)
                            (eq? (node-head-name form) 'define-module))
                          top))
         (edits
          (append
           (append-map
            (lambda (form)
              (let ((specs (form-arguments form)))
                (if (null? specs)
                  ;; an import left with nothing in it is removed rather than
                  ;; written as `(import)', which no Scheme reads
                  (list (list (form-start form)
                              (import-line-end text (form-end form))
                              ""))
                  (list (list (form-start form) (form-end form)
                              (organize-import-form text form specs))))))
            forms)
           (append-map (lambda (form) (module-import-edits text form))
                       modules))))
    (replace-spans text edits)))

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

(define* (optimize-imports-action config text #:key (dialect 'scheme) (uri #f)
                                  (version #f))
  "Return the action that removes unused imports, or #f.

The fix `unused-import' already computes is the whole of it: nothing is
renamed, nothing is added, and the result is an import list a reader can still
read as the list of what the file uses."
  (let ((optimized (if (config-ref config 'optimize-imports)
                     (apply-fixes text
                                  (unused-import-warnings-of config text
                                                             dialect uri))
                     text)))
    (and (not (string=? optimized text))
      (json-object
       (field "title" "sift: optimize imports")
       (field "kind" "source.optimizeImports.sift")
       (field "edit"
              (workspace-edit (or uri "") version text 0 (string-length text)
                              optimized))))))

(define* (sort-imports-action config text #:key (dialect 'scheme) (uri #f)
                              (version #f))
  "Return the action that sorts every import form's specs, or #f.

The specs of an @code{(import ...)} and the @code{#:use-module} clauses of a
@code{define-module} are both read; the latter is the shape most Guile and
Guix code uses."
  (let ((sorted (if (config-ref config 'sort-imports)
                  (organize-imports-text text dialect)
                  text)))
    (and (not (string=? sorted text))
      (json-object
       (field "title" "sift: sort imports")
       (field "kind" "source.sortImports.sift")
       (field "edit"
              (workspace-edit (or uri "") version text 0 (string-length text)
                              sorted))))))

(define* (organize-imports-action config text #:key (dialect 'scheme) (uri #f)
                                  (version #f))
  "Return the action that removes unused imports and sorts the rest, or #f.

Two steps, both of them deletions and reorderings of what the author wrote:
the fixes `unused-import' already computes are applied first, then the specs
of every import form are put in order.  Each step is on by default and each
can be turned off in the configuration, which is what lets an editor's own
\"organize imports\" command mean what the project wants it to mean."
  (let* ((pruned (if (config-ref config 'optimize-imports)
                   (apply-fixes text
                                (unused-import-warnings-of config text
                                                           dialect uri))
                   text))
         (organized (if (config-ref config 'sort-imports)
                      (organize-imports-text pruned dialect)
                      pruned)))
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

The answers, in the order an editor should show them: the fixes for the
findings under the cursor, the extractions sift can make without being told
what to call anything, the one rewrite it can make, and the source actions --
fix everything, the two import operations, and organize imports.  @var{only}
is the request's `context.only', so a client that asked for `source' gets the
source actions and nothing else."
  (let* ((warnings (warnings-for config text #:dialect dialect #:uri uri))
         (quickfixes
          (filter-map
           (lambda (warning)
             (and (lint-warning-fix warning)
               (lint-warning-range warning)
               (code-action-of warning text (or uri "") version)))
           (select-warnings warnings start end text)))
         (extractions
          (and (only-accepts? only "refactor.extract")
            (extract-actions text dialect start end
                             #:uri (or uri "") #:version version)))
         (inlines
          (and (only-accepts? only "refactor.inline")
            (inline-actions text dialect start end
                            #:uri (or uri "") #:version version)))
         (rewrite
          (and (only-accepts? only "refactor.rewrite")
            (format-form-action config text (ast-of-text text dialect) start
                                #:dialect dialect #:uri uri #:version version)))
         (fix-all
          (and (only-accepts? only "source.fixAll")
            (fix-all-action config text #:dialect dialect #:uri uri
                            #:version version)))
         (optimize
          (and (only-accepts? only "source.optimizeImports.sift")
            (optimize-imports-action config text #:dialect dialect #:uri uri
                                     #:version version)))
         (sort
          (and (only-accepts? only "source.sortImports.sift")
            (sort-imports-action config text #:dialect dialect #:uri uri
                                 #:version version)))
         (organize
          ;; The standard "organize imports" is the answer to a request that
          ;; named it, or to a client that asked for the `source' family.  A
          ;; request with no preference gets the two actions a reader can tell
          ;; apart, not that pair again under a third name.
          (and (pair? only)
            (only-accepts? only "source.organizeImports")
            (organize-imports-action config text #:dialect dialect #:uri uri
                                     #:version version))))
    (list->vector
     (append
      (if (only-accepts? only "quickfix") quickfixes '())
      (or extractions '())
      (or inlines '())
      (if rewrite (list rewrite) '())
      (if optimize (list optimize) '())
      (if sort (list sort) '())
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

(define (definition-hover text dialect definition)
  "Return the hover for @var{definition}, which is what a jump would land on.

A definition written in a file is described by its own form; a builtin has no
form, and is described by what its implementation says about it."
  (if (definition-form definition)
    (symbol-hover text dialect definition)
    (let ((markdown (stdlib-builtin-markdown (stdlib-for dialect)
                                             (definition-name definition))))
      (and markdown
        (json-object
         (field "contents"
                (json-object (field "kind" "markdown")
                             (field "value" markdown))))))))

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

(define file-operation-globs
  (append (map (lambda (extension) (string-append "**/*" extension))
               lisp-extensions)
          (map (lambda (extension) (string-append "**/*" extension ".gz"))
               lisp-extensions)))

(define (file-operation-filters)
  "Return the files a rename operation may ask this server about."
  (list->vector
   (map (lambda (glob)
          (json-object (field "scheme" "file")
                       (field "pattern"
                              (json-object (field "glob" glob)))))
        file-operation-globs)))

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
           (field "positionEncoding" "utf-16")
           ;; A file rename is a module rename in two of the three dialects,
           ;; and the editor asks before it moves the file.  The filters say
           ;; which files sift has an opinion about.
           (field "workspace"
                  (json-object
                   (field "fileOperations"
                          (json-object
                           (field "willRename"
                                  (json-object
                                   (field "filters" (file-operation-filters))))
                           (field "didRename"
                                  (json-object
                                   (field "filters" (file-operation-filters))))))))))
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
  (progress? session-progress? set-session-progress!)
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
    (set-session-progress! session #f)
    (set-session-config! session config)
    (set-session-reload! session reload)
    (set-session-settings! session #f)
    session))

(define (client-supports-progress? params)
  "Return @code{#t} when the client advertises work-done progress."
  (equal? 'true
          (json-ref (json-ref (json-ref (json-ref params "capabilities")
                                        "window")
                              "workDoneProgress")
                    #f)))

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

;;; --- progress ------------------------------------------------------------
;;;
;;; Indexing a standard library is the one thing sift does that can take a
;;; minute, and a log line is a poor way to say "still working".  A client that
;;; advertises `window.workDoneProgress' gets a progress token: a create
;;; request, a begin, a report per source and per library, and an end.  A
;;; client that does not gets the same lines as log messages, which is the
;;; answer it can show.

(define progress-token "sift-standard-library")

(define (progress-create! out)
  (send! out
         (json-object
          (field "jsonrpc" "2.0")
          (field "id" "sift-progress-create")
          (field "method" "window/workDoneProgress/create")
          (field "params" (json-object (field "token" progress-token))))))

(define (progress-send! out value)
  (send! out
         (json-object
          (field "jsonrpc" "2.0")
          (field "method" "$/progress")
          (field "params"
                 (json-object (field "token" progress-token)
                              (field "value" value))))))

(define (progress-begin! out title)
  (progress-send! out (json-object (field "kind" "begin")
                                   (field "title" title)
                                   (field "cancellable" #f))))

(define (progress-report! out message)
  (progress-send! out (json-object (field "kind" "report")
                                   (field "message" message))))

(define (progress-end! out message)
  (progress-send! out (json-object (field "kind" "end")
                                   (field "message" message))))

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

(define* (publish! config out document #:key (force #f) (compile? #f) (root #f))
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
         (dialect (document-dialect document))
         (uri (document-uri document))
         (warnings (warnings-for config text #:dialect dialect #:uri uri))
         (diagnostics (list->vector
                       (append (map (lambda (warning) (diagnostic-of warning text))
                                    warnings)
                               (compiler-json-diagnostics uri text dialect)))))
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
                            diagnostics)
      ;; The editor has the findings that could be answered at once; the ones
      ;; that had to read the whole file are computed now and published by the
      ;; message loop when they are ready.
      (queue-deep-pass! out config document warnings)
      (when compile?
        (queue-compiler-pass! out config document root)))))

;;; --- the second pass -----------------------------------------------------
;;;
;;; Five rules need the whole file before they can say anything: a call's
;;; arity is about the definition it resolves to, an import is unused only
;;; when the rest of the file has been read, and a literal argument is a
;;; question about a procedure's convention.  They are the ones that would
;;; make a keystroke wait, so they are computed after the findings the editor
;;; can have at once have gone out -- in a thread of their own -- and published
;;; as a second report for the same version.  The editor draws the first
;;; report immediately and replaces it when the second arrives.

(define deep-checkers
  '(call-arity string-literal-argument number-literal-argument
               eq-non-symbol unused-import-from))

(define *lint-lock* (make-mutex))
(define *deep-threads* '())

(define (with-lint-lock thunk)
  "Call @var{thunk} with the linter to itself.

`lint-source' keeps one cache of the lines of the text it is reading, so two
reports computed at once could read each other's line numbers.  One mutex
around a computation that takes milliseconds is the whole of the fix; the
alternative is a wrong line in a finding."
  (with-mutex *lint-lock* (thunk)))

(define (diagnostics-of warnings text)
  "Return @var{warnings} as the diagnostics an editor expects about @var{text}."
  (list->vector (map (lambda (warning) (diagnostic-of warning text)) warnings)))

;;; ===== The compiler's second opinion =====
;;;
;;; The compiler is asked about the text an editor opened or saved, in a thread
;;; of its own, on a temporary copy of that text.  What it said is kept beside
;;; the text it said it about: a publish for the same text includes the answer
;;; without asking again, and a publish for a different text is the findings
;;; sift can state alone.  The editor gets the fast report first and the
;;; compiler's report when the process exits.

(define compiler-source-limit 200000)
(define *compiler-lock* (make-mutex))
;; One compiler process at a time: a save is a person typing, not a build
;; farm, and two implementations expanding macros at once is a laptop that
;; gets warm while the first answer is still on its way.
(define *compiler-run-lock* (make-mutex))
(define *compiler-diagnostics* (make-hash-table))
(define *compiler-in-flight* (make-hash-table))

(define (compiler-cache-key text dialect)
  (list text dialect))

(define (cached-compiler-diagnostics uri text dialect)
  "Return the compiler findings cached for @var{uri} at @var{text}, or #f.

The empty list is a cache hit: a compiler that had nothing to say is worth
remembering, or every publish would start the same process again."
  (with-mutex *compiler-lock*
              (let ((entry (hash-ref *compiler-diagnostics* uri)))
                (and entry (equal? (car entry) (compiler-cache-key text dialect))
                  (cdr entry)))))

(define (remember-compiler-diagnostics! uri text dialect diagnostics)
  (with-mutex *compiler-lock*
              (hash-set! *compiler-diagnostics* uri
                         (cons (compiler-cache-key text dialect) diagnostics))))

(define (forget-compiler-diagnostics! uri)
  (with-mutex *compiler-lock*
              (hash-remove! *compiler-diagnostics* uri)
              (hash-remove! *compiler-in-flight* uri)))

(define (compiler-in-flight? uri text dialect)
  (with-mutex *compiler-lock*
              (equal? (hash-ref *compiler-in-flight* uri #f)
                      (compiler-cache-key text dialect))))

(define (begin-compiler-run! uri text dialect)
  (with-mutex *compiler-lock*
              (hash-set! *compiler-in-flight* uri (compiler-cache-key text dialect))))

(define (end-compiler-run! uri)
  (with-mutex *compiler-lock* (hash-remove! *compiler-in-flight* uri)))

(define (compiler-severity severity)
  "Return the protocol's number for a compiler @var{severity}."
  (case severity
    [(error) 1]
    [(warning) 2]
    [else 3]))

(define (compiler-diagnostic-json diagnostic)
  "Return a compiler finding as the diagnostic an editor expects."
  (json-object
   (field "range"
          (json-object
           (field "start"
                  (json-object
                   (field "line" (compiler-diagnostic-line diagnostic))
                   (field "character" (compiler-diagnostic-column diagnostic))))
           (field "end"
                  (json-object
                   (field "line" (compiler-diagnostic-end-line diagnostic))
                   (field "character" (compiler-diagnostic-end-column diagnostic))))))
   (field "severity" (compiler-severity (compiler-diagnostic-severity diagnostic)))
   (field "code" (symbol->string (compiler-diagnostic-tool diagnostic)))
   (field "source" (symbol->string (compiler-diagnostic-tool diagnostic)))
   (field "message" (compiler-diagnostic-message diagnostic))))

(define (compiler-json-diagnostics uri text dialect)
  "Return the compiler findings cached for @var{uri} at @var{text}, as JSON."
  (map compiler-diagnostic-json
       (or (cached-compiler-diagnostics uri text dialect) '())))

(define (queue-compiler-pass! out config document root)
  "Ask @var{dialect}'s compiler about @var{document} in a thread of its own.

Only an open or a save asks: a compile is a process, and a process per
keystroke is a server that spends its time starting compilers.  A text that
was compiled before is not compiled again, and a text with a compile already
in flight is not compiled twice -- a save during a slow compile would
otherwise queue a second one behind it."
  (when (config-ref config 'compiler-diagnostics)
    (let* ((text (document-text document))
           (version (document-version document))
           (uri (document-uri document))
           (dialect (document-dialect document))
           (path (and (string-prefix? "file://" uri) (strip-uri uri))))
      (unless (or (> (string-length text) compiler-source-limit)
                (not (compiler-available? dialect))
                (cached-compiler-diagnostics uri text dialect)
                (compiler-in-flight? uri text dialect))
        (begin-compiler-run! uri text dialect)
        (set! *deep-threads*
              (cons
               (call-with-new-thread
                (lambda ()
                  (catch #t
                         (lambda ()
                           (let ((found (with-mutex *compiler-run-lock*
                                                    (compile-text text
                                                                  #:dialect dialect
                                                                  #:file path
                                                                  #:root root))))
                             (remember-compiler-diagnostics! uri text dialect found)
                             (end-compiler-run! uri)
                             (when (and (string=? text (document-text document))
                                     (equal? version (document-version document)))
                               (let ((all (all-warnings config text
                                                        #:dialect dialect
                                                        #:uri uri)))
                                 (with-mutex
                                  *send-lock*
                                  (set! *deep-results*
                                        (cons
                                         (list
                                          uri version
                                          (list->vector
                                           (append
                                            (map (lambda (warning)
                                                   (diagnostic-of warning text))
                                                 all)
                                            (map compiler-diagnostic-json found)))
                                          text)
                                         *deep-results*)))))))
                         (lambda (key . arguments)
                           (end-compiler-run! uri)
                           #f))))
               *deep-threads*))))))

(define *deep-results* '())

(define (warm-standard-libraries! out session)
  "Read the standard library in a thread of its own, and say what it found.

Reading a library is a second of work for a small one and a great deal more for
a machine with a thousand installed packages, and the editor is waiting for the
first jump rather than for this.  Nothing is written from the thread but one
frame through the same lock every other writer takes, so a report about the
library cannot land in the middle of a report about a buffer.

A client that advertised progress gets a progress token and one report per
source and per library; a client that did not gets the same lines in its log."
  (let ((progress? (session-progress? session)))
    (when progress?
      (with-mutex *send-lock* (progress-create! out)))
    (let ((thread
           (call-with-new-thread
            (lambda ()
              (catch #t
                     (lambda ()
                       (when progress?
                         (with-mutex *send-lock*
                                     (progress-begin! out "Indexing the standard library")))
                       (let ((found
                              (stdlib-warm!
                               #:report
                               (lambda (message)
                                 (with-mutex
                                  *send-lock*
                                  (if progress?
                                    (progress-report! out message)
                                    (notice! out message)))))))
                         (when progress?
                           (with-mutex
                            *send-lock*
                            (progress-end!
                             out
                             (format #f "indexed ~a librar~a"
                                     (length found)
                                     (if (= (length found) 1) "y" "ies")))))))
                     (lambda (key . arguments)
                       (when progress?
                         (with-mutex *send-lock*
                                     (progress-end! out "indexing failed")))
                       #f))))))
      (set! *deep-threads* (cons thread *deep-threads*))
      #t)))

(define (queue-deep-pass! out config document fast)
  "Compute the whole-file rules of @var{document} in a thread of their own.

Nothing is written from that thread.  What it finds is handed back through
`*deep-results*', and the message loop -- the one writer a port needs --
publishes it: two threads writing to one port are two chances to put one
frame's bytes inside another's, and an editor that cannot parse the stream
drops the connection rather than the message.

Only a report that says more than the first one is queued.  The findings the
editor already has are not news, and a second `publishDiagnostics' carrying
the same vector is a flicker in every buffer sift checks.  What makes the
report worth sending is a whole-file rule having something to add.

A report is published only for the text it was computed from: a buffer that
changed while the pass ran has findings of its own by now, and a report about
an older text is one an editor would either drop or, worse, show."
  (let ((text (document-text document))
        (version (document-version document))
        (uri (document-uri document))
        (dialect (document-dialect document)))
    (set! *deep-threads*
          (cons (call-with-new-thread
                 (lambda ()
                   (catch #t
                          (lambda ()
                            (let ((all (all-warnings config text
                                                     #:dialect dialect
                                                     #:uri uri))
                                  (compiled (compiler-json-diagnostics
                                             uri text dialect)))
                              (when (and (> (+ (length all) (length compiled))
                                            (length fast))
                                      (string=? text (document-text document))
                                      (equal? version (document-version document)))
                                (with-mutex *send-lock*
                                            (set! *deep-results*
                                                  (cons (list uri version
                                                              (list->vector
                                                               (append
                                                                (map (lambda (warning)
                                                                       (diagnostic-of warning text))
                                                                     (sort all
                                                                           lint-warning<?))
                                                                compiled))
                                                              text)
                                                        *deep-results*))))))
                          (lambda (key . arguments) #f))))
                *deep-threads*))))

(define (flush-deep-results! out documents)
  "Publish the second reports the deep passes have finished.

A report is about the text it was computed from, and a buffer whose text has
changed since has findings of its own by now.  Checking that in the pass's own
thread is not enough: the check and the write are two moments, and this runs
in the message loop, where a `didChange' can be handled between them -- so a
pass that finished while the loop was waiting for the next message would have
its report published *after* the clean one, drawing the old findings back over
the text that no longer has them.  The check is made here as well, against the
document as it is when the report is about to go out, so the only report that
is published is one about the text the editor is showing."
  (let ((pending (with-mutex *send-lock*
                             (let ((results *deep-results*))
                               (set! *deep-results* '())
                               results))))
    (for-each (lambda (result)
                (let* ((uri (car result))
                       (version (cadr result))
                       (diagnostics (caddr result))
                       (text (cadddr result))
                       (document (hash-ref documents uri #f)))
                  (when (and document
                          (string=? text (document-text document))
                          (equal? version (document-version document)))
                    (publish-diagnostics! out uri version diagnostics))))
              (reverse pending))))

(define (join-deep-passes!)
  "Wait for the second passes still running, so an exit does not lose one."
  (for-each (lambda (thread)
              (catch #t
                     (lambda () (join-thread thread))
                     (lambda (key . arguments) #f)))
            *deep-threads*)
  (set! *deep-threads* '()))

(define (finish! out status documents)
  "Return @var{status} once the second passes still running have finished."
  (join-deep-passes!)
  (flush-deep-results! out documents)
  status)

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
       (set-session-progress! session (client-supports-progress? (params)))
       ;; The root is where a question about \"the project\" is answered from;
       ;; an editor that names none still gets its open buffers searched.
       (set-workspace-root! workspace (workspace-root-of (params)))
       ;; Settings an editor sends with `initialize' are the first layer
       ;; above the files, and they stay until it sends others.
       (set-session-settings! session
                              (settings-object-of (params) "initializationOptions"))
       (set-session-config! session
                            (settings-config session out (session-config session)))
       ;; The standard library is where a name the project does not define
       ;; usually lives, and whether to read one at all is a setting: `stdlib'
       ;; is a switch, a list of directories, or nothing at all.
       (stdlib-install! (config-ref (session-config session) 'stdlib #t)
                        #:download? (config-ref (session-config session)
                                                'download-sources #f))
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
       (when (config-ref (session-config session) 'stdlib #t)
         (warm-standard-libraries! out session))
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
         (publish! config out document #:compile? #t
                   #:root (workspace-root workspace))
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
         (forget-compiler-diagnostics! uri)
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
           (publish-all! config out documents #:force #t)
           (queue-compiler-pass! out config document (workspace-root workspace))))
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

      [(string=? method "workspace/willRenameFiles")
       ;; The editor asks before it moves the file; the answer keeps the
       ;; module name, the imports that name it and the feature the file
       ;; provides in step.  A dialect whose module is not a file answers
       ;; nothing, which is the truth about it rather than a guess.
       (let* ((files (json-ref (params) "files" (vector)))
              (edits (append-map
                      (lambda (file)
                        (file-rename-edits workspace documents
                                           (json-ref file "oldUri" "")
                                           (json-ref file "newUri" "")))
                      (vector->list files))))
         (reply! out id (if (pair? edits) (workspace-edit-of edits) json-null)))
       'continue]

      [(string=? method "workspace/didRenameFiles")
       ;; The move happened.  The index still thinks the old path is a file
       ;; and the new one is not, so the old entry goes and the new one is
       ;; read; every open buffer is checked again because a finding can name
       ;; either path.
       (let ((files (json-ref (params) "files" (vector))))
         (for-each
          (lambda (file)
            (let ((old-path (strip-uri (json-ref file "oldUri" "")))
                  (new-path (strip-uri (json-ref file "newUri" ""))))
              (when (workspace-scanned? workspace)
                (hash-remove! (workspace-files workspace) old-path)
                (when (file-exists? new-path)
                  (workspace-refresh! workspace new-path)))))
          (vector->list files))
         (publish-all! config out documents #:force #t))
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
                     (or (and site (definition-hover (cadr site) dialect (caddr site)))
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
       (let ((document (document-of))
             (kinds (config-ref config 'inlay-hints '(parameters))))
         (if (not (and document (pair? kinds)))
           (reply! out id (vector))
           (let* ((text (document-text document))
                  (span (range-of-request text (params))))
             (reply! out id
                     (lsp-inlay-hints workspace documents
                                      (document-uri document) text
                                      (document-dialect document)
                                      (car span) (cdr span)
                                      #:kinds kinds)))))
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
        ;; The workspace is given the globs the project configured, and the
        ;; scanner installed at the bottom of the disk section fills it; a
        ;; workspace with no root is never scanned at all.
        (workspace (new-workspace (and config (config-ref config 'file-filter))))
        (session (new-session #:config config #:reload reload)))
    (let loop ((status 0))
      (flush-deep-results! port-out documents)
      (let ((frame (read-message port-in)))
        (cond
          [(not frame) (finish! port-out status documents)]
          [(eq? (car frame) 'malformed)
           ;; The id of a message that could not be parsed is not known, and
           ;; the protocol says to answer with null.
           (reply-error! port-out json-null -32700 (cdr frame))
           (loop status)]
          [else
            (let ((outcome (serve-one session workspace documents
                                      (cdr frame) port-out)))
              (if (eq? outcome 'exit)
                (finish! port-out (if (session-shutdown? session) 0 1) documents)
                (loop status)))])))))
