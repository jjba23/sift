;; sift --- the standard library, on disk and as documented
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A jump to `car' has nowhere to land inside the project, and a server that
;; answered "not here" would be answering about one in every three names a
;; reader asks about.  So the library a dialect ships with itself is indexed
;; too, from two places, because a standard library comes in two shapes.
;;
;; The first is *source*: Guile's `ice-9', `srfi' and `scheme' modules, the
;; Emacs Lisp files an installation keeps beside its binary, the systems in a
;; Common Lisp source registry, and the package trees those implementations
;; install third-party code into.  Those are files, and they are indexed the
;; way a project's files are -- read, parsed, and asked the same question -- so
;; `(ice-9 format)'s `format' lands on the `define' that defines it, an Emacs
;; package lands on the source it installed, and the hover is the hover of any
;; other definition.  A source that is compressed -- the `.el.gz' most Emacs
;; installations ship -- is decompressed as it is read.
;;
;; The second is what an implementation *knows about itself*: `car' is compiled
;; into libguile and no file in the installation says where.  Each
;; implementation is asked once, in one process, to write down every name it
;; documents, and the answer becomes a source file in that dialect -- a
;; `define' with a docstring for a procedure, a `define-macro' for a macro, a
;; `define' documented by the comment above it for a value.  So a jump to
;; `car' lands in code that reads like the dialect rather than in a list of
;; prose, and the index over the file is the same index a project's files get.
;;
;; Racket is the one dialect that has the first and not the second: it ships
;; `collects' and `pkgs' as source, so its library is read like a project, and
;; a name that only the compiled `#%kernel' defines is the one it cannot
;; answer.  Not shipping a Racket program to describe it is a decision and not
;; an omission -- the files are already there.
;;
;; Nothing here downloads anything.  A standard library that is not installed
;; is a standard library sift cannot read, and the answer is a smaller answer
;; rather than a wrong one; `stdlib:' in `sift.yaml' is how a reader who
;; unpacked a source tree somewhere hands it over.
;;
;; Two costs are worth naming.  Asking an implementation costs one process per
;; dialect and indexing a standard library costs one read of it; both happen
;; once per installation and are cached under the user's cache directory.  The
;; cache is keyed on the implementation's own binary, so an upgraded Guile is
;; read again rather than answered about from an index of the old one.

(define-module (sift stdlib)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift definitions)
  #:use-module (sift files)
  #:use-module (sift sources)
  #:use-module (sift json)
  #:use-module (sift analysis)
  #:export (cache-home
            metadata-of
            without-nested-roots
            index-standard-library
            standard-library?
            standard-library-dialect
            stdlib-definitions
            stdlib-names
            stdlib-summary
            stdlib-for
            stdlib-warm!
            stdlib-install!
            resolve-configured-roots!
            source-cache-directory
            stdlib-forget!))

;;; ===== Caches =====

(define (darwin?)
  "Return @code{#t} when this is macOS, where a cache lives in ~/Library."
  (let ((uname (false-if-exception (uname))))
    (and uname (string=? (vector-ref uname 0) "Darwin"))))

(define (cache-home)
  "Return the directory sift keeps its caches in, or @code{#f} when it has none.

`XDG_CACHE_HOME' wins because it is the user saying where; macOS gets its own
`Library/Caches' because that is where a Mac application's cache belongs, and
`LOCALAPPDATA' is the same answer for a Windows shell."
  (let ((xdg (getenv "XDG_CACHE_HOME"))
        (home (getenv "HOME"))
        (local (getenv "LOCALAPPDATA")))
    (cond
      [(and xdg (not (string-null? xdg))) (string-append xdg "/sift")]
      [(and home (not (string-null? home)) (darwin?))
       (string-append home "/Library/Caches/sift")]
      [(and home (not (string-null? home))) (string-append home "/.cache/sift")]
      [(and local (not (string-null? local))) (string-append local "/sift")]
      [else #f])))

(define (cache-directory)
  "Return the directory a standard library's index and index file live in."
  (let ((home (cache-home)))
    (and home (string-append home "/stdlib"))))

(define (source-cache-directory)
  "Return the directory downloaded source archives and trees live in."
  (let ((home (cache-home)))
    (and home (string-append home "/sources"))))

;; 2: a compressed source is read and the package trees a dialect installs
;; third-party code into are roots.  An index written before that names too
;; few files to answer the question it is asked.
(define cache-version 3)

(define* (make-directories path)
  "Create @var{path} and every missing parent, like @code{mkdir -p}."
  (unless (or (file-exists? path) (string=? path "/") (string-null? path))
    (make-directories (dirname path))
    (false-if-exception (mkdir path))))

;;; ===== What a standard library is =====
;;;
;;; The records come first, before the code that reads one into existence: a
;;; record's constructor is a syntax transformer, and a procedure written above
;;; it would be compiled against a name that is not bound yet.

;; A library carries one index: the names its files define, kept as a hash
;; because it is asked one name at a time, and a name is the only thing a
;; request has.  What has been read is kept beside it: a jump into a standard
;; library file parses it once however many times the editor asks about it.
(define-record-type <standard-library>
  (make-standard-library dialect implementation roots names
                         builtins-path texts definitions offered)
  standard-library?
  (dialect standard-library-dialect)
  (implementation standard-library-implementation)
  (roots standard-library-roots)
  (names standard-library-names)
  (builtins-path standard-library-builtins-path)
  (texts standard-library-texts)
  (definitions standard-library-definitions)
  (offered standard-library-offered set-standard-library-offered!))

;;; ===== The file an implementation described itself in =====
;;;
;;; The second half of a library is the names an implementation compiled in:
;;; `car' is in libguile and no installed file defines it.  sift asks the
;;; implementation once and writes the answer as *source* -- a `define' with a
;;; docstring for a procedure, a `define-macro' for a macro, and a `define'
;;; whose documentation is the comment above it for a value -- so a jump to
;;; `car' lands in code that reads like the dialect, and the index over it is
;;; the same index a project's files get.  The `;; sift:' lines at the top say
;;; which implementation wrote the file and which directories to read.

(define (metadata-of text)
  "Return the `;; sift: key value' lines of a generated index, as an alist."
  (let loop ((lines (string-split text #\newline)) (metadata '()))
    (cond
      [(null? lines) (reverse metadata)]
      [(string-prefix? ";; sift: " (car lines))
       (let* ((body (substring (car lines) 9))
              (space (string-index body #\space)))
         (loop (cdr lines)
               (if space
                 (cons (cons (substring body 0 space)
                             (string-trim-both (substring body (+ space 1))))
                       metadata)
                 metadata)))]
      [else (loop (cdr lines) metadata)])))

(define (metadata-value metadata key)
  (let ((found (assoc key metadata)))
    (and found (cdr found))))

(define (metadata-values metadata key)
  (map cdr (filter (lambda (entry) (equal? (car entry) key)) metadata)))

;;; ===== Which directories a library is in =====

(define (directory? path)
  (eq? 'directory (false-if-exception (stat:type (stat path)))))

(define (canonical path)
  "Return @var{path} with every symlink followed, or @var{path} when it cannot be.

A profile directory is a symlink to a store path, so `~/.guix-profile/share/guile'
and the store path it points at are the same library under two names: walking
both would index it twice and answer with two places for one definition.
  "
  (or (and (string? path) (false-if-exception (canonicalize-path path))) path))

(define (existing-directories paths)
  "Return the strings among @var{paths} that are directories, without repeats."
  (delete-duplicates
   (filter directory?
           (map (lambda (path)
                  (and (string? path)
                    (not (string-null? path))
                    (canonical path)))
                paths))
   string=?))

(define (without-nested-roots paths)
  "Return @var{paths} without the ones another of @var{paths} already holds.

A `%load-path' names a directory and its parent -- `/share/guile/3.0' and
`/share/guile' -- and walking both would read every file twice and answer with
two places for one definition, so the parent is kept and the child is dropped."
  (filter (lambda (path)
            (not (any (lambda (other)
                        (and (< (string-length other) (string-length path))
                          (string-prefix? other path)
                          (char=? #\/ (string-ref path (string-length other)))))
                      paths)))
          paths))

;;; ===== Reading a standard library's files =====

;; A standard library is larger than a project and is read once per
;; installation, so the limits are generous and bounded: a source registry plus
;; every installed package costs one background read, not a request.
(define file-limit 10000)
(define byte-limit 500000)
(define generated-byte-limit 16000000)

(define (lisp-file? extensions path)
  "Return @code{#t} when @var{path} ends in one of @var{extensions}.

A compressed source is read through its suffix: the Emacs library is installed
as `.el.gz', and the dialect list names `.el'."
  (let ((path (without-compression path)))
    (any (lambda (extension) (string-suffix? extension path)) extensions)))

(define (files-under-roots roots extensions)
  "Return the files under @var{roots} whose name ends in one of @var{extensions}.

The walk is breadth-first and bounded, and it never enters a directory whose
name begins with a dot: the answer is a name index, and a `.git' or a
`guile-ccache' beside the sources holds no definitions."
  (define (walk pending found count)
    (cond
      [(or (null? pending) (>= count file-limit)) (reverse found)]
      [else
        (let ((here (car pending))
              (deeper '())
              (files '()))
          (for-each
           (lambda (name)
             (unless (string-prefix? "." name)
               (let ((path (string-append here "/" name)))
                 (cond
                   [(directory? path) (set! deeper (cons path deeper))]
                   [(lisp-file? extensions path) (set! files (cons path files))]))))
           (or (false-if-exception (scandir here)) '()))
          (walk (append (reverse deeper) (cdr pending))
                (append files found)
                (+ count (length files))))]))
  (walk roots '() 0))

(define (definitions-of-text text dialect)
  "Return the definitions of @var{text}, read as @var{dialect}."
  (call-with-values
    (lambda () (read-source #:source text #:dialect dialect))
    (lambda (ast errors) (parse-definitions ast))))

(define (record-file! names dialect path text)
  "Add the top-level definitions of @var{text}, read as @var{dialect}, to @var{names}."
  (false-if-exception
   (for-each
    (lambda (definition)
      (unless (definition-scope definition)
        (let* ((name (definition-name definition))
               (found (hash-ref names name)))
          (hash-set! names name
                     (cons (cons path (definition-kind definition))
                           (or found '()))))))
    (definitions-of-text text dialect))))

(define (index-files dialect paths)
  "Return a hash from the name each of @var{paths} defines to where it does.

The name is read by parsing the file, which is the same reading the project's
own files get: a standard library is not a special case, it is a library.
A file sift cannot read is skipped rather than fatal -- an installation is full
of files it has never seen the dialect of, and one of them must not answer for
all of them."
  (let ((names (make-hash-table)))
    (for-each
     (lambda (path)
       (let ((text (read-file-text path byte-limit)))
         (when text (record-file! names dialect path text))))
     paths)
    names))

;;; ===== A library a caller has in hand =====

(define* (index-standard-library #:key dialect roots (builtins-path #f)
                                 (implementation "unknown"))
  "Return the standard library @var{roots} and @var{builtins-path} describe.

Nothing is discovered, nothing is written and no implementation is asked: this
is the shape both the discovery and a test fill in, which is what lets the
reading of a library be tested without one.  @var{builtins-path} is the file
the implementation described itself in, if it wrote one; it is read with the
same reader as @var{roots}, and with a limit that fits a file holding every
name an implementation documents."
  (let* ((roots (without-nested-roots (existing-directories roots)))
         ;; the generated file may sit inside a root the reader named, and it
         ;; is read once, with the limit it was written under
         (names (index-files dialect
                             (remove (lambda (path) (equal? path builtins-path))
                                     (files-under-roots
                                      roots (dialect-extensions dialect))))))
    (when (and builtins-path (file-exists? builtins-path))
      (let ((text (read-file-text builtins-path generated-byte-limit)))
        (when text (record-file! names dialect builtins-path text))))
    (make-standard-library dialect implementation roots names builtins-path
                           (make-hash-table) (make-hash-table) #f)))

(define (library-text library path)
  "Return the text of @var{path}, read once per library.

The generated file is larger than a source file because it holds every name an
implementation documents, so it is read with the limit it was written under."
  (let ((texts (standard-library-texts library))
        (limit (if (equal? path (standard-library-builtins-path library))
                 generated-byte-limit
                 byte-limit)))
    (or (hash-ref texts path)
      (let ((text (read-file-text path limit)))
        (when text (hash-set! texts path text))
        text))))

(define (library-definitions library path)
  "Return the definitions of @var{path}, read once per library."
  (let ((definitions (standard-library-definitions library)))
    (or (hash-ref definitions path)
      (let* ((text (library-text library path))
             (read (and text
                     (false-if-exception
                      (definitions-of-text text
                                           (standard-library-dialect library))))))
        (when read (hash-set! definitions path read))
        read))))

(define (bare-name name)
  "Return @var{name} without the marker a dialect writes in front of it."
  (cond
    [(string-prefix? "#:" name) (substring name 2)]
    [(string-prefix? ":" name) (substring name 1)]
    [else name]))

(define (name-parts name)
  "Return the spellings of @var{name} an index may hold.

A name carrying a package marker is written `app:run' and indexed as `run', so
both are tried before the answer is nothing."
  (let* ((bare (bare-name name))
         (colon (string-rindex bare #\:)))
    (delete-duplicates (if colon
                         (list bare (substring bare (+ colon 1)))
                         (list bare))
                       string=?)))

(define (site-rank site builtins-path)
  "Return how good an answer @var{site} is, lowest first.

Actual code comes before the file an implementation described itself in, and a
definition that writes its parameters -- `(define (format port . args) ...)'
-- comes before a value: a jump should land where a reader can read around it.
The generated file is ranked last rather than excluded, because it is code
too; it is simply the answer of last resort, and a project, a required library
or the installation's own sources beat it."
  (let* ((definition (caddr site))
         (formals (definition-formals definition)))
    (cond
      [(and builtins-path (equal? (car site) (path->uri builtins-path))) 2]
      [(pair? formals) 0]
      [else 1])))

(define (ordered-sites sites builtins-path)
  "Return @var{sites} with the answers a reader wants first, in file order."
  (stable-sort sites (lambda (a b)
                       (< (site-rank a builtins-path)
                          (site-rank b builtins-path)))))

(define (stdlib-definitions library name)
  "Return @code{(uri text definition)} for every place @var{library} defines @var{name}.

Every place, ordered by what a reader asking the question would rather be shown:
a definition that writes its parameters, then a definition whose value is
something else, then the file the implementation described itself in.  The
order is stable, so the files stay in the order the walk found them."
  (if (standard-library? library)
    (let ((parts (name-parts name)))
      (ordered-sites
       (append-map
        (lambda (entry)
          (let ((path (car entry)))
            (let ((text (library-text library path))
                  (definitions (library-definitions library path)))
              (if text
                (map (lambda (definition)
                       (list (path->uri path) text definition))
                     (filter (lambda (definition)
                               (and (not (definition-scope definition))
                                 (member (definition-name definition) parts)))
                             (or definitions '())))
                '()))))
        (append-map (lambda (part)
                      (or (hash-ref (standard-library-names library) part) '()))
                    parts))
       (standard-library-builtins-path library)))
    '()))

(define (stdlib-names library)
  "Return @code{(name . kind)} for every name @var{library} knows.

A name is offered with the kind its definition has, which is the kind the
editor colours it with; the generated file states one the same way a project's
file does."
  (if (standard-library? library)
    (or (standard-library-offered library)
      (let ((names '()))
        (hash-for-each
         (lambda (name entries)
           (set! names (cons (cons name (cdr (car entries))) names)))
         (standard-library-names library))
        (let ((offered (reverse names)))
          (set-standard-library-offered! library offered)
          offered)))
    '()))

(define (stdlib-summary library)
  "Return one line saying what @var{library} holds, or @code{#f} when it holds nothing."
  (and (standard-library? library)
    (format #f "~a: ~a, ~a names"
            (standard-library-dialect library)
            (standard-library-implementation library)
            (hash-fold (lambda (name entries count) (+ count 1)) 0
                       (standard-library-names library)))))

;;; ===== Asking the implementation =====
;;;
;;; Each of the three is asked in its own words, once, and told where to write
;;; through the environment rather than the command line: a trailing argument
;;; means something different to each of them, and the path is the one thing
;;; they must not disagree about.

;;; Each script writes *source*: the dialect's own definition form, with the
;;; documentation where that dialect keeps it.  The answer is read back by the
;;; same reader and the same index a project's files get, so a jump lands on a
;;; `define' rather than on a line of prose.

(define guile-script
  "(use-modules (ice-9 documentation) (ice-9 format) (ice-9 session)
             (srfi srfi-1) (srfi srfi-13))
(define (formals-of arguments)
  (if (not (pair? arguments))
    \"...\"
    (let ((required (or (assq-ref arguments 'required) '()))
          (optional (or (assq-ref arguments 'optional) '()))
          (rest (assq-ref arguments 'rest)))
      (string-append
       \"(\"
       (string-join
        (remove string-null?
                (append (map symbol->string required)
                        (if (null? optional)
                          '()
                          (cons \"#:optional\" (map symbol->string optional)))
                        (if rest (list \".\" (symbol->string rest)) '())))
        \" \")
       \")\"))))
(define (comment-lines text)
  (string-join (map (lambda (line) (string-append \";; \" line))
                    (string-split text #\\newline))
               \"\\n\"))
(define (write-entry port name value doc)
  (cond
    ((procedure? value)
     (format port \"(define (~a ~a)\\n  ~s\\n  #f)\\n\\n\"
             name (formals-of (procedure-arguments value)) doc))
    ((false-if-exception (macro? value))
     (format port \"(define-macro (~a . args)\\n  ~s\\n  #f)\\n\\n\" name doc))
    (else
     ;; Scheme has no docstring slot for a value, so the comment above the
     ;; definition carries the prose and a reader still lands on the `define'.
     (format port \"~a\\n(define ~a #f)\\n\\n\" (comment-lines doc) name))))
(let ((target (getenv \"SIFT_STDLIB_OUT\")))
  (when target
    (call-with-output-file target
      (lambda (port)
        (format port \";; sift --- the names GNU Guile ~a documents about itself\\n\" (version))
        (format port \";;\\n;; Written by asking the implementation once.  sift's language server reads\\n\")
        (format port \";; this file like any other source file: a procedure is a `define' with a\\n\")
        (format port \";; docstring, a macro is a `define-macro', and a value is a `define' whose\\n\")
        (format port \";; documentation is the comment above it.\\n;;\\n\")
        (format port \";; sift: version ~a\\n\" (version))
        ;; A user's own Scheme libraries are the third-party half of what a
        ;; project imports; the profile directories are on `%load-path' only
        ;; when the shell put them there.
        (for-each
         (lambda (path)
           (when (and (string? path) (file-exists? path)
                      (not (member path %load-path)))
             (set! %load-path (append %load-path (list path)))))
         (list (string-append (or (getenv \"HOME\") \"\")
                              \"/.local/share/guile/site/3.0\")
               (string-append (or (getenv \"HOME\") \"\")
                              \"/.guix-profile/share/guile/site/3.0\")
               (string-append (or (getenv \"HOME\") \"\")
                              \"/.config/guix/current/share/guile/site/3.0\")))
        (for-each (lambda (path)
                    (format port \";; sift: root ~a\\n\" path))
                  %load-path)
        (format port \"\\n\")
        (module-map
         (lambda (name variable)
           (let ((value (false-if-exception
                         (and (variable-bound? variable) (variable-ref variable)))))
             (when value
               (let ((doc (false-if-exception (object-documentation value))))
                 (when (and (string? doc) (not (string-null? doc)))
                   (write-entry port name value doc))))))
         (resolve-module '(guile)))))))
")

(define elisp-script
  "(let ((target (getenv \"SIFT_STDLIB_OUT\")))
  (when target
    (with-temp-file target
      (insert (format \";; sift --- the names GNU Emacs %s documents about itself\\n\" emacs-version))
      (insert \";;\\n;; Written by asking the implementation once.  sift's language server reads\\n\")
      (insert \";; this file like any other Emacs Lisp file: a procedure is a `defun' with\\n\")
      (insert \";; a docstring, a macro a `defmacro', and a value a `defvar'.\\n;;\\n\")
      (insert (format \";; sift: version %s\\n\" emacs-version))
      ;; Third-party packages live below `package-user-dir', and `-Q' ran no
      ;; init file to put them or the system's site-lisp on `load-path'.  The
      ;; directories are added whole: the index walk is recursive, and its
      ;; roots are de-duplicated so a child of another root is not read twice.
      (dolist (directory (list (and (boundp 'package-user-dir)
                                    (expand-file-name package-user-dir))
                               (expand-file-name \"~/.config/emacs/elpa\")
                               (expand-file-name \"~/.emacs.d/elpa\")
                               (expand-file-name \"../../site-lisp\"
                                                 data-directory)
                               \"/usr/local/share/emacs/site-lisp\"
                               \"/usr/share/emacs/site-lisp\"))
        (when (and (stringp directory) (file-directory-p directory)
                   (not (member directory load-path)))
          (setq load-path (append load-path (list directory)))))
      (dolist (directory load-path)
        (insert (format \";; sift: root %s\\n\" directory)))
      (insert \"\\n\")
      (mapatoms
       (lambda (symbol)
         (let ((doc (condition-case nil (documentation symbol) (error nil))))
           (when (and (stringp doc) (> (length doc) 0))
             (let ((arguments (condition-case nil
                                  (help-function-arglist symbol t)
                                (error nil))))
               ;; an arglist that is not loaded yet comes back as prose, and a
               ;; string is not a parameter list
               (unless (listp arguments)
                 (setq arguments '(&rest args)))
               (cond
                 ((macrop symbol)
                  (insert (format \"(defmacro %S %S\\n  %S\\n  nil)\\n\\n\"
                                  symbol (or arguments '(&rest args)) doc)))
                 ((fboundp symbol)
                  (insert (format \"(defun %S %S\\n  %S\\n  nil)\\n\\n\"
                                  symbol (or arguments '(&rest args)) doc)))
                 (t
                  (insert (format \"(defvar %S nil\\n  %S)\\n\\n\" symbol doc))))))))))
      (insert \"(provide 'sift-builtins)\\n\"))))
")

(define sbcl-script
  "(let ()
  (require :sb-introspect)
  (let ((target (sb-ext:posix-getenv \"SIFT_STDLIB_OUT\"))
        ;; looked up by name rather than read as
        ;; `sb-introspect:function-lambda-list', because a package the require
        ;; did not find cannot be read at all
        (lambda-list (find-symbol \"FUNCTION-LAMBDA-LIST\" \"SB-INTROSPECT\")))
    (when target
      (with-open-file (stream target
                              :direction :output
                              :if-exists :supersede
                              :external-format :utf-8)
        (format stream \";; sift --- the names SBCL ~a documents about itself~%\"
                (lisp-implementation-version))
        (format stream \";;~%;; Written by asking the implementation once.  sift's language server~%\")
        (format stream \";; reads this file like any other Common Lisp file: a procedure is a~%\")
        (format stream \";; `defun' with a docstring, a macro a `defmacro', and a value a~%\")
        (format stream \";; `defvar'.~%;;~%\")
        (format stream \";; sift: version ~a~%\" (lisp-implementation-version))
        (format stream \"~%\")
        (dolist (package-name '(\"COMMON-LISP\" \"SB-EXT\"))
          (let ((package (find-package package-name)))
            (when package
              (do-external-symbols (symbol package)
                (let* ((function-doc (ignore-errors
                                      (documentation symbol 'function)))
                       (variable-doc (and (not function-doc)
                                          (ignore-errors
                                           (documentation symbol 'variable))))
                       (text (or function-doc variable-doc)))
                  (when (and text (> (length text) 0))
                    (cond
                      (function-doc
                       (format stream \"(~a ~(~a~) ~a~%  ~s~%  nil)~%~%\"
                               (if (macro-function symbol) \"defmacro\" \"defun\")
                               symbol
                               (or (and lambda-list
                                     (ignore-errors
                                      ;; named one by one: an argument list as
                                      ;; SBCL prints it carries the package each
                                      ;; symbol lives in, and `SB-IMPL::NAME' is
                                      ;; a name to SBCL and noise to a reader
                                      (format nil \"(~{~a~^ ~})\"
                                              (mapcar
                                               (lambda (argument)
                                                 (if (symbolp argument)
                                                     (string-downcase
                                                      (symbol-name argument))
                                                     (string-downcase
                                                      (format nil \"~a\" argument))))
                                               (funcall lambda-list symbol)))))
                                   \"(&rest args)\")
                               text))
                      (variable-doc
                       (format stream \"(defvar ~(~a~) nil~%  ~s)~%~%\" symbol text)))))))))))))
")

(define dialects
  '((scheme (extensions . (".scm" ".ss" ".sld" ".sls" ".sps"))
            (program . "guile")
            (arguments . ("-q" "--no-auto-compile" "-c"))
            (name . "GNU Guile"))
    (elisp (extensions . (".el"))
           (program . "emacs")
           (arguments . ("--batch" "-Q" "--eval"))
           (name . "GNU Emacs"))
    (cl (extensions . (".lisp" ".lsp" ".cl" ".asd"))
        (program . "sbcl")
        ;; `--noinform' is a runtime option, so it comes before the Lisp ones: in
        ;; the middle of them SBCL refuses to start at all.
        (arguments . ("--noinform" "--non-interactive" "--eval"))
        (name . "SBCL"))
    ;; Racket has a program and no script: it ships `collects' and `pkgs' as
    ;; source, so its library is read rather than asked for.  `.scrbl' is
    ;; Racket too, but a Scribble file documents a name rather than defining
    ;; one, and the package trees ship thousands of them.
    (racket (extensions . (".rkt" ".rktd" ".rktl"))
            (program . "racket")
            (name . "Racket"))))

(define (dialect-spec dialect)
  (let ((entry (assq dialect dialects)))
    (and entry (cdr entry))))

(define (dialect-script dialect)
  "Return the program @var{dialect} is asked to describe itself with.

The scripts are held apart from the table above because the table is quoted
data and a script is a string: a `(script . guile-script)' member of a quoted
list holds the symbol, which is a program that writes nothing."
  (case dialect
    [(scheme) guile-script]
    [(elisp) elisp-script]
    [(cl) sbcl-script]
    [else #f]))

(define (dialect-extensions dialect)
  (let ((spec (dialect-spec dialect)))
    (and spec (cdr (assq 'extensions spec)))))

(define (program-file program)
  "Return the file @var{program} names, whether it is a name or a path.

A name is looked for on @code{PATH} and a path is taken as it is: the resolved
path is what the index is keyed on, and asking the resolver to resolve its own
answer would find nothing."
  (cond
    [(string-contains program "/") (and (file-exists? program) program)]
    [else
      (let ((path (getenv "PATH")))
        (and path
          (find (lambda (candidate) (file-exists? candidate))
                (map (lambda (directory) (string-append directory "/" program))
                     (string-split path #\:)))))]))

(define* (run-to-file program arguments target)
  "Run @var{program} with @var{arguments} so that it writes @var{target}.

The output goes to a file rather than through a pipe because it is to be read
as one: a standard library's documentation is megabytes, and the path arrives
in the environment because a trailing argument is a different thing to each of
the three implementations.  What the child says on its own account is drained
and dropped -- a server that printed it would print inside a protocol frame."
  (catch #t
         (lambda ()
           (let ((previous (getenv "SIFT_STDLIB_OUT")))
             (dynamic-wind
               (lambda () (setenv "SIFT_STDLIB_OUT" target))
               (lambda ()
                 (let ((port (apply open-pipe* OPEN_READ program arguments)))
                   (get-string-all port)
                   (let ((status (close-pipe port)))
                     (let ((exit (false-if-exception (status:exit-val status))))
                       (and exit (zero? exit))))))
               (lambda ()
                 (if previous
                   (setenv "SIFT_STDLIB_OUT" previous)
                   (unsetenv "SIFT_STDLIB_OUT"))))))
         (lambda (key . arguments) #f)))

;;; ===== Where a standard library is =====

(define (home-path suffix)
  (let ((home (getenv "HOME")))
    (and home (not (string-null? home)) (string-append home "/" suffix))))

(define (common-lisp-roots)
  "Return the directories a Common Lisp installation keeps its systems in.

A source registry is a convention rather than a file: ASDF looks in a handful
of places, and so does this, because asking ASDF would mean starting an
implementation to answer a question about a path."
  (existing-directories
   (append (map (lambda (directory)
                  (string-append directory "/common-lisp/source"))
                (append (filter (lambda (directory) (not (string-null? directory)))
                                (string-split (or (getenv "XDG_DATA_DIRS") "") #\:))
                        (list "/usr/local/share" "/usr/share")))
           (list (home-path ".quicklisp/dists/quicklisp/software")
                 (home-path "quicklisp/dists/quicklisp/software")
                 (home-path "quicklisp/local-projects")
                 (home-path ".roswell/lisp/quicklisp/dists/quicklisp/software")
                 (home-path ".local/share/common-lisp/source")
                 "/usr/local/lib/sbcl/contrib"
                 "/usr/lib/sbcl/contrib"))))

(define (racket-roots)
  "Return the directories a Racket installation keeps its library in.

Racket ships `collects' -- the library `#lang racket' is written from -- and
`pkgs', what was installed beside it, as source, so the index is read the way
Guile's is rather than asked for.  The installation's prefix is the directory
above the `racket' the `PATH' names; `PLTCOLLECTS' is the variable Racket
itself reads, and a `--user' package install puts a version's trees under the
data directory."
  (let* ((program (program-file "racket"))
         (prefix (and program
                   (let ((bin (dirname program)))
                     (and (string=? "bin" (basename bin)) (dirname bin))))))
    (existing-directories
     (append
      (if prefix
        (list (string-append prefix "/share/racket/collects")
              (string-append prefix "/share/racket/pkgs"))
        '())
      (append-map
       (lambda (parent)
         (append-map
          (lambda (version)
            (list (string-append parent "/" version "/collects")
                  (string-append parent "/" version "/pkgs")))
          (or (false-if-exception (scandir parent)) '())))
       (filter directory? (list (home-path ".local/share/racket")
                                (home-path ".racket"))))
      (string-split (or (getenv "PLTCOLLECTS") "") #\:)))))

(define (discovered-roots dialect metadata)
  "Return the directories @var{dialect}'s library is in, as @var{metadata} says."
  (cond
    [(eq? dialect 'cl) (common-lisp-roots)]
    ;; Racket is asked nothing, so it names no root: discovery says where its
    ;; installation shipped its source instead.
    [(eq? dialect 'racket) (racket-roots)]
    [else (existing-directories (metadata-values metadata "root"))]))

(define (implementation-name dialect metadata)
  "Return a sentence naming the implementation that wrote @var{metadata}."
  (let ((name (cdr (assq 'name (dialect-spec dialect))))
        (version (metadata-value metadata "version")))
    (if version
      (format #f "~a ~a" name version)
      (or name "unknown"))))

(define (builtins-file dialect)
  "Return the file @var{dialect}'s implementation describes itself in.

The file is source in the dialect it describes -- `.scm' for Scheme, `.el' for
Emacs Lisp, `.lisp' for Common Lisp -- because it is read by the same reader as
a project's files and a jump into it should land on code."
  (let ((directory (cache-directory))
        (extensions (dialect-extensions dialect)))
    (and directory (dialect-script dialect) (pair? extensions)
      (format #f "~a/~a-builtins~a" directory dialect (car extensions)))))

(define (index-file dialect)
  (let ((directory (cache-directory)))
    (and directory (format #f "~a/~a.json" directory dialect))))

;;; ===== The index, kept between sessions =====
;;;
;;; Reading a standard library costs what a project costs, and it is read for
;;; the whole installation rather than for one checkout, so it is written down.
;;; What makes the file trustworthy is not a timestamp but the implementation's
;;; own binary: an Emacs that was upgraded has a different file behind the
;;; name, and an index of the old one would answer about names that moved.

(define (program-signature program)
  "Return @code{(path mtime size)} for @var{program}, or @code{#f}."
  (let ((path (program-file program)))
    (and path
      (let ((attributes (false-if-exception (stat path))))
        (and attributes
          (list path (stat:mtime attributes) (stat:size attributes)))))))

(define (written-index dialect program)
  "Return the index of @var{dialect} on disk when it is still about @var{program}."
  (let ((path (index-file dialect))
        (signature (and program (program-signature program))))
    (and path signature (file-exists? path)
      (let ((document (false-if-exception
                       (call-with-input-file
                        path (lambda (port) (json-read (get-string-all port)))))))
        (and (json-object? document)
          (equal? (json-ref document "version") cache-version)
          (equal? (json-ref document "dialect") (symbol->string dialect))
          (equal? (json-ref document "program") (car signature))
          (equal? (json-ref document "mtime") (cadr signature))
          (equal? (json-ref document "size") (caddr signature))
          document)))))

(define (temporary-path path)
  "Return a name beside @var{path} this process can write and then rename.

A file is written under one of these and renamed into place, so that a session
that ends in the middle of writing leaves the index it had rather than half of
the index it was making."
  (format #f "~a.partial-~a" path (getpid)))

(define (write-index! dialect library program)
  "Write @var{library} down as the index of @var{dialect}, if it can be."
  (let ((path (index-file dialect))
        (signature (program-signature program)))
    (when (and path signature)
      (make-directories (dirname path))
      (let ((partial (temporary-path path)))
        (false-if-exception
         (call-with-output-file
          partial
          (lambda (port)
            (display (json-write
                      (json-object
                       (field "version" cache-version)
                       (field "dialect" (symbol->string dialect))
                       (field "implementation"
                              (standard-library-implementation library))
                       (field "program" (car signature))
                       (field "mtime" (cadr signature))
                       (field "size" (caddr signature))
                       (field "roots"
                              (list->vector (standard-library-roots library)))
                       (field "files"
                              (list->vector
                               (hash-map->list
                                (lambda (name entries)
                                  (json-object
                                   (field "name" name)
                                   (field "kind"
                                          (symbol->string (cdr (car entries))))
                                   (field "paths"
                                          (list->vector (map car entries)))))
                                (standard-library-names library))))))
                     port))))
        ;; the index itself was written whole or not at all: a file renamed
        ;; into place is a file the next session can read
        (false-if-exception (rename-file partial path))))))

(define (names-from-json files)
  "Return the name index a stored @var{files} array describes."
  (let ((names (make-hash-table)))
    (for-each
     (lambda (entry)
       (let ((name (json-ref entry "name" #f))
             (kind (json-ref entry "kind" "variable"))
             (paths (json-ref entry "paths" (vector))))
         (when name
           (hash-set! names name
                      (map (lambda (path)
                             (cons path (string->symbol kind)))
                           (vector->list paths))))))
     (vector->list files))
    names))

(define (library-of-index dialect document)
  "Return the library a stored index describes."
  (make-standard-library dialect
                         (json-ref document "implementation" "unknown")
                         (vector->list (json-ref document "roots" (vector)))
                         (names-from-json (json-ref document "files" (vector)))
                         (builtins-file dialect)
                         (make-hash-table)
                         (make-hash-table)
                         #f))

(define (stored-library dialect program)
  "Return @var{dialect}'s library from its index, or @code{#f} when there is none."
  (let ((document (written-index dialect program)))
    (and document (library-of-index dialect document))))

;;; ===== Asking for one =====

(define (index-and-write! dialect program generated roots implementation)
  "Index @var{roots} as @var{dialect}'s library and write the index down.

@var{generated} is the file the implementation described itself in, or
@code{#f} for a dialect that is read from source alone."
  (let ((library (index-standard-library #:dialect dialect
                                         #:roots roots
                                         #:builtins-path generated
                                         #:implementation implementation)))
    (write-index! dialect library program)
    library))

(define (build-library dialect program override)
  "Read @var{dialect}'s library, asking its implementation when there is one.

@var{override} is a list of directories to read instead of the ones the
implementation names, which is how a source tree a reader unpacked themselves
is handed over.  Racket has no script to ask -- its library ships as source --
so the files are the whole answer."
  (let ((script (dialect-script dialect)))
    (make-directories (cache-directory))
    (if script
      (let ((generated (builtins-file dialect))
            (arguments (cdr (assq 'arguments (dialect-spec dialect)))))
        (and generated
          ;; what is written beside the index is moved into place, so that a
          ;; run that fails leaves the one it had
          (let ((partial (temporary-path generated)))
            (and (run-to-file program (append arguments (list script)) partial)
              (let ((text (read-file-text partial generated-byte-limit)))
                (and text
                  (begin
                    (false-if-exception (rename-file partial generated))
                    (index-and-write!
                     dialect program generated
                     (or override (discovered-roots dialect (metadata-of text)))
                     (implementation-name dialect (metadata-of text))))))))))
      (index-and-write! dialect program #f
                        (or override (discovered-roots dialect '()))
                        (implementation-name dialect '())))))

;; The libraries are asked for by dialect and kept for the session: the index
;; is the same for every project in the editor, and reading it once is what
;; makes a jump into it as cheap as a jump into the buffer.
;;
;; Two tables and not one, because the first read of a library takes what a
;; project takes -- a second, and on a machine with a thousand installed
;; libraries a good deal more -- and the server asks this question in the
;; middle of an editor's request.  A dialect being read is a dialect whose
;; answer is "not yet", which the caller already handles; the thread that read
;; it is the one that says so when it is done.
(define *libraries* (make-hash-table))
(define *reading* (make-hash-table))
(define *library-lock* (make-mutex))
(define configured-roots #f)
;; What the setting named, before URLs were resolved: directories as they were
;; written and URLs as they will be fetched.
(define configured-sources #f)
(define download-sources? #f)
(define stdlib-enabled? #f)

(define* (resolve-configured-roots! #:key (report #f))
  "Resolve the URL entries of the `stdlib' setting, and answer the directories.

A URL is downloaded and unpacked only when `download-sources' is true; a
directory is itself.  The resolved list becomes the roots `read-library'
indexes, so a failure is one source missing rather than a library that cannot
be read."
  (if (not (and stdlib-enabled? configured-sources))
    '()
    (let* ((local (filter (lambda (entry) (not (source-url? entry)))
                          configured-sources))
           (urls (filter source-url? configured-sources))
           (cache (source-cache-directory))
           (downloaded
            (cond
              [(null? urls) '()]
              [(not download-sources?)
               (when report
                 (report "sources: URLs in stdlib need download-sources: true"))
               '()]
              [(not cache)
               (when report (report "sources: no cache directory"))
               '()]
              [else
                (filter-map (lambda (url)
                              (resolve-source url cache #:report report))
                            urls)])))
      (set! configured-roots (append local downloaded))
      downloaded)))

(define (read-library dialect)
  "Read @var{dialect}'s library, or answer @code{#f} when it cannot be read."
  (let ((spec (dialect-spec dialect)))
    (and spec
      (let* ((program (program-file (cdr (assq 'program spec))))
             (library (and program
                        (or (and (not configured-roots)
                              (stored-library dialect program))
                          (build-library dialect program configured-roots)))))
        ;; A reader who named the directories sift should read gets those
        ;; directories even when no implementation is installed to document
        ;; them: the sources are the half that answers a jump.
        (or library
          (and configured-roots
            (index-standard-library #:dialect dialect
                                    #:roots configured-roots)))))))

(define (index-on-disk? dialect)
  "Return @code{#t} when @var{dialect}'s index is written down already.

What that means for a request is the difference between reading one file and
adding up a library: the first is a request that waits, and the second is one
that is answered without the library until the thread reading it is done."
  (let ((index (index-file dialect))
        (generated (and (dialect-script dialect) (builtins-file dialect))))
    (and index (file-exists? index)
      (or (not generated) (file-exists? generated)))))

(define (stdlib-for dialect)
  "Return @var{dialect}'s standard library, reading it on first use.

An index that is already written down is read here and now: one file, sooner
than a request can be handed to another thread.  An index that has to be built
is built by one thread, and a request that arrives meanwhile is answered
without it -- with the smaller answer the caller already handles -- rather than
made to wait for a library it may not even be asking about."
  (or (with-mutex *library-lock* (hash-ref *libraries* dialect))
    (cond
      [(index-on-disk? dialect)
       (let ((library (read-library dialect)))
         (when library
           (with-mutex *library-lock* (hash-set! *libraries* dialect library)))
         library)]
      [(with-mutex *library-lock*
                   (unless (hash-ref *reading* dialect)
                     (hash-set! *reading* dialect #t)
                     #t))
       (let ((library (read-library dialect)))
         (with-mutex *library-lock*
                     (when library (hash-set! *libraries* dialect library))
                     (hash-remove! *reading* dialect))
         library)]
      [else #f])))

(define* (stdlib-warm! #:key (report #f))
  "Read every dialect's library, and return a line about each that was read.

Called in a thread of its own by the server, so that the first jump a reader
makes into a standard library is a jump and not a wait.  @var{report} is called
with a line as each source is resolved and each library is read, which is what
lets an editor show that the work is happening rather than that sift is idle."
  (if (not stdlib-enabled?)
    '()
    (begin
      (resolve-configured-roots! #:report report)
      (filter-map (lambda (dialect)
                    (when report
                      (report (format #f "indexing the ~a library" dialect)))
                    (let ((library (stdlib-for dialect)))
                      (and library (pair? (stdlib-names library))
                        (let ((summary (stdlib-summary library)))
                          (when report (report summary))
                          summary))))
                  (map car dialects)))))

(define (stdlib-definitions-for dialect name)
  "Return the places @var{dialect}'s standard library defines @var{name}.

Nothing a standard library does may fail a request: an index that cannot be
read, a file that went away and an implementation that raised all answer the
same way, which is with the empty list the caller already handles."
  (catch #t
         (lambda ()
           (let ((library (stdlib-for dialect)))
             (if library (stdlib-definitions library name) '())))
         (lambda (key . arguments) '())))

(define* (stdlib-completions dialect prefix #:optional (limit 200))
  "Return @code{(name . kind)} from @var{dialect}'s library matching @var{prefix}.

The names are the ones worth offering for what is being typed, which is what
keeps a completion request over a twenty-thousand-name library as quick as one
over a buffer."
  (catch #t
         (lambda ()
           (let ((library (stdlib-for dialect))
                 (needle (string-downcase prefix)))
             (if library
               (let loop ((names (stdlib-names library)) (found '()))
                 (cond
                   [(or (null? names) (>= (length found) limit)) (reverse found)]
                   [(string-contains (string-downcase (car (car names))) needle)
                    (loop (cdr names) (cons (car names) found))]
                   [else (loop (cdr names) found)]))
               '())))
         (lambda (key . arguments) '())))

;;; ===== Installing =====
;;;
;;; The analysis knows the question and not where the answer comes from, so what
;;; it is handed here is a procedure and not a path.  A build with no
;;; filesystem -- or a reader who turned the setting off -- hands it nothing,
;;; and every answer about the project stays the answer it was.

(define* (stdlib-install! setting #:key (download? #f))
  "Answer the analysis's questions about the standard library from here on.

@var{setting} is the configuration's `stdlib': @code{#t} to discover what this
machine has installed, a list of directories or source URLs to read instead,
or @code{#f} to answer nothing about a library at all.  @var{download?} is the
`download-sources' setting: a URL is fetched only when it is true, so a project
file cannot make sift reach the network on its own."
  (set! configured-sources (and (list? setting) setting))
  (set! download-sources? download?)
  (set! stdlib-enabled? (and setting #t))
  (set! configured-roots
        (and (list? setting)
          (filter (lambda (entry) (not (source-url? entry))) setting)))
  (hash-clear! *libraries*)
  (if setting
    (begin
      (current-stdlib stdlib-definitions-for)
      (current-stdlib-names stdlib-completions))
    (begin
      (current-stdlib (lambda (dialect name) '()))
      (current-stdlib-names (lambda (dialect prefix) '())))))

(define (stdlib-forget! dialect)
  "Forget what this session knows about @var{dialect}, and its index with it."
  (hash-remove! *libraries* dialect)
  (let ((index (index-file dialect))
        (generated (builtins-file dialect)))
    (when index (false-if-exception (delete-file index)))
    (when generated (false-if-exception (delete-file generated))))
  #t)
