;; sift --- what a language server answers about a workspace
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; This is the half of `(sift lsp)' that is a function of a buffer, a cursor and
;; a set of indexed files: positions, documents, the workspace record,
;; definition, references, highlights, the outline, completion, rename, hover,
;; signature help and inlay hints.  The other half -- the JSON-RPC wire, the
;; server loop, the diagnostics, the code actions and the reading of a project
;; off a disk -- stayed there, and needs all of this.
;;
;; It is a module of its own because a second kind of caller exists: one that
;; holds a single buffer and no project at all.  Nothing here touches a file --
;; a workspace is filled by a scan the caller installs, an indexed file is
;; loaded by a loader the caller installs, and a build with neither answers
;; about what it was handed.  `(sift lsp)' installs both, because it has a
;; directory to read; a caller that has none installs nothing and passes an
;; empty workspace, and gets the same answers about the one buffer it has.
;;
;; The questions are the same questions either way, which is the point: an
;; editor in a tab and an editor beside a checkout get one implementation of
;; "where is this defined", not two that agree until they do not.

(define-module (sift analysis)
  ;; Not declarative, for the reason the server is not: the record accessors
  ;; here are handed to `map' and kept beside the records they describe, and an
  ;; accessor from a declarative module is only inlinable, not a value.
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift definitions)
  #:use-module (sift check base)
  #:use-module (sift checkers inference)
  #:use-module (sift strings)
  #:use-module (sift json)
  #:export (;; --- json members, shared with the server ---
            field

            ;; --- positions ---
            json-range
            position-of
            document-range
            index-of-text-position
            range-of-request
            client-version
            strip-uri
            dialect-of

            ;; --- documents ---
            <document>
            new-document
            document-uri
            document-text
            set-document-text!
            document-language-id
            document-version
            set-document-version!
            document-dialect
            document-published
            set-document-published!

            ;; --- the workspace ---
            <workspace>
            new-workspace
            workspace-index
            workspace-root
            set-workspace-root!
            workspace-filter
            set-workspace-filter!
            workspace-files
            workspace-scanned?
            set-workspace-scanned!
            current-workspace-scan
            <indexed-file>
            make-indexed-file
            indexed-file?
            indexed-file-path
            indexed-file-text
            set-indexed-file-text!
            indexed-file-dialect
            indexed-file-definitions
            set-indexed-file-definitions!
            indexed-file-names
            indexed-file-text-of
            indexed-file-definitions-of
            current-index-loader
            workspace-definition-sites
            workspace-names
            path->uri
            percent-encode
            workspace-root-of

            ;; --- what an editor asks ---
            ast-of-text
            form-start
            form-end
            definition-sites
            lsp-definition
            lsp-references
            lsp-document-highlights
            lsp-document-symbols
            workspace-symbols
            completion-items
            rename-edit
            text-document-of
            workspace-edit
            valid-symbol-name?
            symbol-hover
            lsp-signature-help
            lsp-inlay-hints
            extract-actions))

;;; ===== The wire =====
;;;
;;; The one piece of the server's vocabulary that the answers are written in: a
;;; JSON object member.  It lives here because the answers are objects, and the
;;; server takes it from here rather than keeping a second copy.

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

(define-record-type <workspace>
  (make-workspace root files scanned? filter)
  workspace?
  (root workspace-root set-workspace-root!)
  (files workspace-files)
  (scanned? workspace-scanned? set-workspace-scanned!)
  (filter workspace-filter set-workspace-filter!))

(define* (new-workspace #:optional (filter #f))
  "Return an empty workspace, with @var{filter} for the globs a project names.

A workspace starts with no root and nothing in it; what fills it is
`current-workspace-scan', which a build with a directory to read installs."
  (make-workspace #f (make-hash-table) #f filter))

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

;;; An indexed file can be *lazy*: the index remembers a file's names and not
;;; its text, and the text arrives when a request needs it.  How it arrives is
;;; not this module's business -- the server reads the file from the project,
;;; and a build with no files to read loads nothing -- so it is a parameter,
;;; and the answer to "no" is an empty one rather than an error.

(define current-index-loader
  (make-parameter
   (lambda (entry)
     "Load @var{entry} from wherever this build keeps its files, or do nothing."
     #f)))

(define (indexed-file-text-of entry)
  "Return the text of @var{entry}, asking the loader when it is not loaded."
  (or (indexed-file-text entry)
    (begin ((current-index-loader) entry)
      (indexed-file-text entry))))

(define (indexed-file-definitions-of entry)
  "Return the definitions of @var{entry}, asking the loader when they are not read."
  (or (indexed-file-definitions entry)
    (begin ((current-index-loader) entry)
      (indexed-file-definitions entry))))

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

;;; A workspace is an index and, at the caller's option, a way to fill it.  The
;;; server fills it from a directory; a caller with no directory to read hands
;;; over an empty one, and every answer is about the buffers it was given.

(define current-workspace-scan
  (make-parameter
   (lambda (workspace)
     "Fill @var{workspace} however this build fills one, or do nothing."
     #f)))

(define (workspace-index workspace)
  "Return the workspace's index, asking the scanner to fill it the first time.

The scan and its contents are the caller's: what is promised here is that it
happens once, and that a workspace nobody can scan is an empty index rather
than an error."
  (unless (workspace-scanned? workspace)
    ((current-workspace-scan) workspace)
    (set-workspace-scanned! workspace #t))
  (workspace-files workspace))

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
              [(string-find downcased needle) 1]
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
        (when (string-find (string-downcase name) needle)
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
    (and (every-char (lambda (character)
                       (not (or (char-whitespace? character)
                              (memv character
                                    '(#\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\, #\; #\| #\#)))))
                     name)
      #t)))

(define (text-document-of uri version)
  "Return the `textDocument' of a workspace edit, with a version when known.

An edit that names the version it was computed from is applied only while the
buffer still is that version, which is the difference between renaming a
symbol and renaming whatever is in that spot now."
  (if (and version (number? version))
    (json-object (field "uri" uri) (field "version" version))
    (json-object (field "uri" uri))))

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
  "Return the hover that describes a definition rather than a rule.

A procedure is described by what it takes as well as by what it is: the form
is what the outline shows, and the sentence beside it is the same reading of
the parameter list the arity rule makes, so a hover and a finding cannot
disagree about how many arguments a definition wants."
  (let* ((dialect-name (symbol->string dialect))
         (signature (and (memq (definition-kind definition) '(function method))
                      (definition-form definition)
                      (form-signature (definition-form definition))))
         (summary (and signature
                    (arity-summary (cons (car signature) (cadr signature))
                                   dialect))))
    (json-object
     (field "contents"
            (json-object
             (field "kind" "markdown")
             (field "value"
                    (format #f "**~a** — ~a~%~%```~a~%~a~%```~@[~%~%~a~]~@[~%~%~a~]"
                            (definition-name definition)
                            (definition-kind definition)
                            dialect-name
                            (symbol-detail definition text)
                            summary
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

(define (parameter-label parameter text dialect)
  "Return one parameter as a signature writes it.

A keyword is spelled the way the dialect spells one, because that is the word
a caller types: `#:height' in Guile, `:height' in Common Lisp."
  (let ((kind (car parameter))
        (node (cdr parameter)))
    (case kind
      [(key)
       (string-append (if (eq? dialect 'scheme) "#:" ":")
                      (symbol->string (or (and (node-atom? node) (atom-symbol node))
                                        'key)))]
      [else
        (cond
          [(not node) ""]
          [(node-atom? node) (node-value node)]
          [else (substring text (form-start node)
                           (min (form-end node) (string-length text)))])])))

(define (parameter-position parameters name)
  "Return where the keyword called @var{name} is written, or 0."
  (or (list-index (lambda (parameter)
                    (and (eq? (car parameter) 'key)
                      (let ((node (cdr parameter)))
                        (and (node-atom? node)
                          (equal? (atom-symbol node) name)))))
                  parameters)
    0))

(define (active-parameter call index parameters)
  "Return which parameter of @var{call} the cursor at @var{index} is writing.

An argument the cursor has not left yet is the one being written; a keyword
places it at that keyword's own parameter, which is what makes the editor
underline `#:height' while its value is typed rather than the first argument."
  (let loop ((rest (form-arguments call)) (position 0))
    (cond
      [(null? rest)
       (if (pair? parameters) (min position (- (length parameters) 1)) 0)]
      [(keyword-node? (car rest))
       (let ((keyword (car rest))
             (value (and (pair? (cdr rest)) (cadr rest))))
         (cond
           [(<= index (form-end keyword))
            (parameter-position parameters (atom-symbol keyword))]
           [(and value (<= index (form-end value)))
            (parameter-position parameters (atom-symbol keyword))]
           [else (loop (cddr rest) position)]))]
      [(<= index (form-end (car rest)))
       (and (pair? parameters) (min position (- (length parameters) 1)))]
      [else (loop (cdr rest) (+ position 1))])))

(define* (lsp-signature-help workspace documents uri text dialect index)
  "Return the signature of the call the cursor is in, or @code{#f}.

The signature is the one sift can know: the definition's parameter list, read
the way the arity rule reads it, so the editor shows the keyword names, the
optional and rest parameters, and which of them is being written.  A procedure
from a library sift cannot read has no signature here, and answering with an
invented one would be worse than answering nothing."
  (let* ((ast (ast-of-text text dialect))
         (call (call-at ast index)))
    (and call
      (let ((head (node-head call)))
        (let* ((sites (definition-sites workspace documents uri text dialect
                                        (source-location-index (node-start head))))
               (site (and (pair? sites) (car sites)))
               (definition (and site (caddr site)))
               (definition-text (and site (cadr site)))
               (signature (and definition (definition-form definition)
                            (form-signature (definition-form definition))))
               (parameters (and signature (caddr signature))))
          (and (pair? parameters)
            (let* ((name (definition-name definition))
                   (labels (map (lambda (parameter)
                                  (parameter-label parameter definition-text dialect))
                                parameters))
                   (label (format #f "(~a ~a)" name (string-join labels " ")))
                   (arity (cons (car signature) (cadr signature)))
                   (summary (arity-summary arity dialect))
                   (documentation (documentation-text
                                   (definition-docstring definition))))
              (json-object
               (field "signatures"
                      (vector
                       (json-object
                        (field "label" label)
                        (field "parameters"
                               (list->vector
                                (map (lambda (parameter-text)
                                       (json-object (field "label" parameter-text)))
                                     labels)))
                        (field "documentation"
                               (json-object
                                (field "kind" "markdown")
                                (field "value"
                                       (if (string? documentation)
                                         documentation
                                         summary)))))))
               (field "activeSignature" 0)
               (field "activeParameter" (active-parameter call index parameters))))))))))

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

;;; ===== Refactors =====
;;;
;;; Two extractions, and both are the same three steps: find the expression the
;;; cursor is in, name it, and write the name where the expression was.  What
;;; differs is where the expression goes -- into a `let' binding around the
;;; form that held it, or into a procedure beside the definition it came from.
;;;
;;; A refactor that has to be told what to call things is a refactor nobody
;;; runs, so the name is read from the expression: the operator of a call names
;;; what it computes, and the fallback names anything that does not.  A name
;;; the document already uses is not reused -- `area' becomes `area-2' -- so an
;;; extraction cannot shadow the procedure the call came from.

(define (form-at ast index)
  "Return two values: the innermost form of @var{ast} holding @var{index}, and
its ancestors, nearest first."
  (define (inside? node)
    (and (<= (form-start node) index)
      (< index (max (form-start node) (form-end node)))))
  (define (descend node ancestors)
    (let ((child (find (lambda (candidate)
                         (and (node-data? candidate) (inside? candidate)))
                       (if (node-list? node) (node-elements node) '()))))
      (if child
        (descend child (cons node ancestors))
        (values node ancestors))))
  (let loop ((nodes ast))
    (cond
      [(null? nodes) (values #f '())]
      [(and (node-data? (car nodes)) (inside? (car nodes)))
       (descend (car nodes) '())]
      [else (loop (cdr nodes))])))

(define (expression-at ast index)
  "Return two values: the expression a request at @var{index} is about, and
its ancestors.

An atom is an expression, but the head of a call is not what a reader means by
\"this expression\": `(compute-area w h)' with the cursor on `compute-area' is
a request about the call, so the call is what an extraction works on."
  (call-with-values (lambda () (form-at ast index))
    (lambda (node ancestors)
      (if (and node (pair? ancestors)
            (eq? node (node-head (car ancestors))))
        (values (car ancestors) (cdr ancestors))
        (values node ancestors)))))

(define (symbol-names node)
  "Return every symbol @var{node} writes, in source order."
  (cond
    [(not node) '()]
    [(node-atom? node)
     (if (eq? (node-atom-type node) 'symbol) (list (node-value node)) '())]
    [(node-list? node) (append-map symbol-names (node-elements node))]
    [(node-prefix? node) (symbol-names (node-child node))]
    [else '()]))

(define (document-names ast)
  "Return every symbol the document writes, for choosing a name nobody uses."
  (delete-duplicates (append-map symbol-names (filter node-data? ast))))

(define (parameter-names-of form)
  "Return the names a definition form binds as parameters."
  (filter-map (lambda (node)
                (and (node-atom? node)
                  (eq? (node-atom-type node) 'symbol)
                  (node-value node)))
              (and form (definition-formals form))))

(define (let-names-of form)
  "Return the names a `let' form binds, or @code{'()}."
  (let ((arguments (form-arguments form)))
    (and (pair? arguments)
      (let* ((named (and (node-atom? (car arguments)) (pair? (cdr arguments))))
             (bindings (if named (cadr arguments) (car arguments))))
        (and (node-list? bindings)
          (filter-map (lambda (binding)
                        (and (node-list? binding)
                          (let ((head (node-head binding)))
                            (and head (node-atom? head) (node-value head)))))
                      (node-elements bindings)))))))

(define (enclosing-names ancestors)
  "Return the names the forms around an expression bind.

A parameter or a `let' variable is what an expression can be pulled out of and
still need: everything else it mentions is either defined at the top level --
still in scope for a procedure written beside it -- or bound inside the
expression itself, and so not an argument."
  (delete-duplicates
   (append-map
    (lambda (ancestor)
      (cond
        [(not (node-list? ancestor)) '()]
        [(memq (node-head-name ancestor) '(let let* letrec letrec*))
         (or (let-names-of ancestor) '())]
        [(definition-form? ancestor)
         (append-map parameter-names-of (parse-definitions (list ancestor)))]
        [else '()]))
    ancestors)))

(define (free-names node ancestors)
  "Return the names @var{node} uses that an enclosing form binds."
  (let ((enclosing (enclosing-names ancestors)))
    (delete-duplicates
     (filter (lambda (name) (member name enclosing)) (symbol-names node)))))

(define (name-from-form node fallback)
  "Return the name an expression suggests, or @var{fallback}.

`(compute-area w h)' computes an area, and `compute-area' is the name a reader
would have chosen; `(* w h)' suggests nothing a variable should be called, so
the fallback is used instead of `*'."
  (let* ((head (and (node-list? node) (node-head node)))
         (text (cond
                 [(and head (node-atom? head)
                    (eq? (node-atom-type head) 'symbol)) (node-value head)]
                 [(and (node-atom? node)
                    (eq? (node-atom-type node) 'symbol)) (node-value node)]
                 [else ""]))
         (clean (list->string
                 (filter (lambda (character)
                           (or (char-alphabetic? character)
                             (char-numeric? character)
                             (char=? character #\-)))
                         (string->list (or text ""))))))
    (if (and (positive? (string-length clean))
          (char-alphabetic? (string-ref clean 0)))
      clean
      fallback)))

(define (fresh-name base used)
  "Return @var{base}, or the first `base-N' that is not in @var{used}."
  (if (not (member base used))
    base
    (let loop ((counter 2))
      (let ((candidate (format #f "~a-~a" base counter)))
        (if (member candidate used) (loop (+ counter 1)) candidate)))))

(define (wrapped-line text)
  "Return @var{text} with every line but the first indented by two spaces."
  (string-join (string-split text #\newline) "\n  "))

(define* (extract-to-let text ast start end #:key (uri "") (version #f))
  "Return the action that binds the selected expression to a name, or #f.

The binding goes around the form the expression is written in -- the smallest
form that still reads with the name where the expression was -- so
`(f (g x) (h y))' with `(g x)' chosen becomes
`(let ((value (g x))) (f value (h y)))'."
  (call-with-values (lambda () (expression-at ast end))
    (lambda (node ancestors)
      (let ((parent (and (pair? ancestors) (car ancestors))))
        (and node parent (node-list? parent)
          (not (definition-form? parent))
          (not (and (formals-holder? parent)
                 (eq? (formals-of-form parent) node)))
          (let* ((excerpt (substring text (form-start node) (form-end node)))
                 (name (fresh-name (name-from-form node "value")
                                   (document-names ast)))
                 (rewritten (string-append
                             (substring text (form-start parent) (form-start node))
                             name
                             (substring text (form-end node) (form-end parent))))
                 (replacement (format #f "(let ((~a ~a)) ~a)"
                                      name excerpt rewritten)))
            (json-object
             (field "title" (format #f "sift: bind this expression to `~a`" name))
             (field "kind" "refactor.extract")
             (field "edit"
                    (workspace-edit uri version text (form-start parent)
                                    (form-end parent) replacement)))))))))

(define* (extract-to-procedure text ast start end #:key (uri "") (version #f))
  "Return the action that moves the selected expression into a procedure, or #f.

The procedure takes what the expression uses and the enclosing forms bind --
its parameters and its `let' variables -- and it is written after the top-level
form the expression came from, which is where the rest of the file's procedures
are.  What the expression uses from the top level stays there: a definition is
in scope from a procedure written beside it, so passing it as an argument would
be a copy nobody asked for."
  (call-with-values (lambda () (expression-at ast end))
    (lambda (node ancestors)
      (let ((top (and (pair? ancestors) (last ancestors))))
        (and node top (not (eq? node top))
          (let* ((excerpt (substring text (form-start node) (form-end node)))
                 (arguments (free-names node ancestors))
                 (name (fresh-name (name-from-form node "extracted")
                                   (document-names ast)))
                 (call (if (null? arguments)
                         (format #f "(~a)" name)
                         (format #f "(~a ~a)" name (string-join arguments " "))))
                 (procedure (format #f "(define (~a~@[ ~a~])~%~a~a)"
                                    name
                                    (and (pair? arguments)
                                      (string-join arguments " "))
                                    "  "
                                    (wrapped-line excerpt)))
                 (edits
                  (vector
                   (json-object
                    (field "range" (json-range text (form-start node)
                                               (form-end node)))
                    (field "newText" call))
                   (json-object
                    (field "range" (json-range text (form-end top) (form-end top)))
                    (field "newText" (string-append "\n\n" procedure))))))
            (json-object
             (field "title"
                    (format #f "sift: extract this expression to the procedure `~a`" name))
             (field "kind" "refactor.extract")
             (field "edit"
                    (json-object
                     (field "documentChanges"
                            (vector
                             (json-object
                              (field "textDocument" (text-document-of uri version))
                              (field "edits" edits)))))))))))))

(define* (extract-actions text dialect start end #:key (uri "") (version #f))
  "Return the extraction actions that apply to @var{start}..@var{end}."
  (let ((ast (ast-of-text text dialect)))
    (filter identity
            (list (extract-to-let text ast start end #:uri uri #:version version)
                  (extract-to-procedure text ast start end
                                        #:uri uri #:version version)))))

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
  "Return the parameter name of the argument at @var{index}, or @code{#f}.

@var{formals} is @code{#f} for a call sift cannot resolve, and then no
argument has a name to put beside it."
  (and (pair? formals) (< index (length formals)) (list-ref formals index)))

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

(define (inlay-hint label position kind padding-left?)
  "Return one `InlayHint' for @var{label} at @var{position}.

@var{kind} is the protocol's: 1 is a type and 2 a parameter, and an editor
that styles hints draws the two differently -- a name beside its argument, a
type in the same place but greyer.  `paddingLeft' asks for a space before the
label, so the hint reads as a word beside the argument instead of as part of
it."
  (json-object
   (field "position" position)
   ;; The gap is in the label as well as in the padding, because a client is
   ;; free to ignore `paddingLeft'/`paddingRight' and most draw the label right
   ;; against the argument when they do: `width' then reads as part of the
   ;; number rather than as a name for it.
   (field "label" (string-append label "  "))
   (field "kind" kind)
   (field "paddingLeft" padding-left?)
   (field "paddingRight" #t)))

;;; --- the type an argument states outright --------------------------------
;;;
;;; sift infers nothing, and a type hint is the one place a hint could look
;;; like inference.  So this answers only where the *source* states the type:
;;; a string literal is a string, `#t' is a boolean, `(lambda ...)' is a
;;; procedure.  An argument that is a name gets no hint, because the only thing
;;; sift could say about it is what the reader already has to know.
;;;
;;; That is also why the setting is off by default: beside `"Main"' a hint
;;; reading `string' is the editor repeating the source, and a hint nobody
;;; needs is worse than no hint at all.

(define (quoted-type node)
  "Return the type a quoted datum states, or @code{#f}."
  (let ((child (node-child node)))
    (and child (node-data? child)
      (cond
        [(node-atom? child) (literal-type child)]
        [(node-list? child) "list"]
        [(node-vector? child) "vector"]
        [else #f]))))

(define (literal-call-type node)
  "Return the type a call whose result is its own definition states, or #f.

Three forms say what they build and nothing else: `lambda' is a procedure,
`list' a list and `vector' a vector.  Nothing else is read this way -- `(cons
1 2)' is a pair, but saying so is sift following a definition it cannot see
rather than reading the form in front of it."
  (case [node-head-name node]
    [(lambda) "procedure"]
    [(list) "list"]
    [(vector) "vector"]
    [else #f]))

(define (literal-type node)
  "Return the type an atom states, or @code{#f}."
  (case [node-atom-type node]
    [(string) "string"]
    [(number) "number"]
    [(character) "character"]
    [(boolean) "boolean"]
    [else #f]))

(define (obvious-type argument)
  "Return the type @var{argument} states outright, or @code{#f}."
  (cond
    [(not (node-data? argument)) #f]
    [(node-atom? argument) (literal-type argument)]
    [(node-prefix? argument) (quoted-type argument)]
    [(node-list? argument) (literal-call-type argument)]
    [(node-vector? argument) "vector"]
    [else #f]))

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

(define* (lsp-inlay-hints workspace documents uri text dialect start end
                          #:key (kinds '(parameters)))
  "Return the inlay hints for the calls of @var{text} between @var{start} and
@var{end}, as the array an editor expects.

The range decides what is answered, not only what is looked at: a hint is
emitted for an argument that *starts* inside it, which is what an editor asks
for -- the part of the file it is showing -- and which keeps a long file from
being resolved a form at a time for hints nobody will see.

The walk is over forms rather than over every list, because a parameter list
is a list and is not a call: `(define (make-window parent width) ...)' has a
`parent' in argument position that nothing is being passed to, and a hint
there would be sift describing a definition as if it were a call to itself.

@var{kinds} is what the configuration asked for: `parameters' names a call's
positional arguments from the definition, and `types' says what type an
argument states outright.  Both can be on at once, and then a reader gets
`width' followed by `number' on the same argument -- the name from the
definition, the type from the source."
  (let ((hints '())
        (ast (ast-of-text text dialect))
        (wanted? (lambda (kind) (memq kind kinds))))
    (for-each-form
     (lambda (node)
       (when (and (call-form? node)
               (let ((head (node-head node)))
                 (and head (node-atom? head)
                   (eq? (node-atom-type head) 'symbol))))
         (let ((arguments (form-arguments node)))
           ;; the cheap question first: is any of this call's arguments in the
           ;; part of the file the editor is showing?
           (when (any (lambda (argument)
                        (let ((index (source-location-index (node-start argument))))
                          (and (>= index start) (<= index end))))
                      arguments)
             (let ((formals (and (wanted? 'parameters)
                              (call-formals ast workspace documents uri text
                                            dialect node))))
               (let loop ((rest arguments) (position 0) (named? #t))
                 (unless (null? rest)
                   (let* ((argument (car rest))
                          (index (source-location-index (node-start argument)))
                          (keyword? (keyword-node? argument))
                          (name (and named? (not keyword?)
                                  (argument-name-at formals position)))
                          (type (and (wanted? 'types) (obvious-type argument)))
                          (here? (and (>= index start) (<= index end))))
                     (when here?
                       ;; the name first: `width' is what the argument is, and
                       ;; `number' is what it is made of
                       (when name
                         (set! hints
                               (cons (inlay-hint name (json-position text index) 2 #t)
                                     hints)))
                       (when type
                         (set! hints
                               (cons (inlay-hint type (json-position text index) 1 #t)
                                     hints))))
                     (loop (cdr rest)
                           (if (or (not named?) keyword?) position (+ position 1))
                           (and named? (not keyword?)))))))))))
     ast)
    (list->vector (reverse hints))))
