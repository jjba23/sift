;; sift --- imports and libraries
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A Scheme program that says what it uses is a program a reader can read from
;; the top: `(import (scheme base) (only (srfi 1) fold))' names two libraries
;; and, in the second, the one name it wants out of it.  Two of sift's rules
;; are about that sentence -- an import nothing uses, and a library no file
;; declares -- and both need the same three things: the import forms read as
;; data, the library forms read as data, and the names a file actually
;; mentions.
;;
;; What is *not* here is as important as what is.  sift does not expand a
;; macro, does not resolve a library's exports beyond the `(export ...)' the
;; library itself writes down, and does not model a library search path.  The
;; consequence is written into each rule rather than hidden: a name is called
;; unused only when the file names it (an `only' or `rename' clause), and a
;; library is called missing only when the project declares libraries at all
;; and this one is not among them -- a project with no `library' form anywhere
;; has nothing to compare an import against, and the rule says nothing.
;;
;; Reading the neighbouring files is the one place sift looks outside the text
;; it was handed, and it is deliberate: "does this library exist" is a question
;; about the project, not about the file.  The scan is bounded (a depth, a file
;; count and a size per file), sorted, and cached per directory, so two files
;; in one directory pay for it once and the answer does not depend on the order
;; the files were linted in.

(define-module (sift imports)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift check base)
  #:export (import-forms
            import-sites
            import-site?
            import-site-form
            import-site-anchor
            import-site-specs
            site-clause?
            declaration-clauses
            export-names-of-nodes
            module-exports
            module-export-clause-names
            file-module
            mentions
            import-specs
            data-elements
            spec-kind
            spec-library
            spec-library-node
            spec-name-nodes
            rename-pair-nodes
            library-forms
            library-name
            library-exports
            library-key
            well-known-library?
            declared-libraries
            project-libraries
            referenced-names
            used?
            deletion-range))

;;; ===== Reading the forms =====

(define (data-elements node)
  "Return the data elements of @var{node}: no whitespace, no comments."
  (if (node-list? node)
    (filter node-data? (node-elements node))
    '()))

(define (element-name node)
  "Return what an atom spells, as a string, or @code{#f}."
  (and node
    (node-atom? node)
    (case [node-atom-type node]
      [(symbol string keyword number) (node-value node)]
      [else #f])))

(define (first-data nodes)
  "Return the first node of @var{nodes} that carries a datum, or @code{#f}."
  (cond
    [(null? nodes) #f]
    [(node-data? (car nodes)) (car nodes)]
    [else (first-data (cdr nodes))]))

(define (declaration-clauses form)
  "Return the @code{(keyword . datum)} clauses of a declaration @var{form}.

`(define-module (foo) #:use-module (srfi srfi-1) #:export (a b))' is a
sequence of keyword clauses with whitespace and comments between them, and the
datum a keyword introduces is what the clause is about.  A `#:'-keyword is a
symbol atom with a package marker, which is why this is not a keyword type test.
"
  (let loop ((rest (node-children form)) (out '()))
    (cond
      [(null? rest) (reverse out)]
      [(keyword-node? (car rest))
       (let ((argument (first-data (cdr rest))))
         (if argument
           (loop (memq argument rest) (cons (cons (car rest) argument) out))
           (loop (cdr rest) out)))]
      [else (loop (cdr rest) out)])))

(define (module-exports form)
  "Return the names a `define-module' @var{form} exports, in order.

`#:export' and `#:re-export' both put a name on the module's public surface:
the first because the module defines it here, the second because it hands on a
name another module defined.  Either way a file that imports the module can
name it."
  (append-map
   (lambda (clause)
     (if (memq (atom-symbol (car clause)) (quote (export re-export)))
       (export-names-of-nodes (data-elements (cdr clause)))
       (quote ())))
   (declaration-clauses form)))

(define (module-export-clause-names form)
  "Return the names a `define-module' @var{form} puts in its `#:export' clause.

@code{#f} is not the same answer as the empty list: a module that says
`#:export ()' has declared that it exports nothing, and a module with no
`#:export' clause has not said what it exports at all -- a script exports
nothing and is right to."
  (let ((clause (find (lambda (pair) (eq? (atom-symbol (car pair)) 'export))
                      (declaration-clauses form))))
    (and clause (export-names-of-nodes (data-elements (cdr clause))))))

(define (file-module ast)
  "Return the `define-module' form of @var{ast}, or @code{#f}."
  (find (lambda (node) (list-headed-by? node 'define-module))
        (filter node-data? ast)))

(define (import-forms ast)
  "Return the @code{(import ...)} forms of @var{ast}, in source order.

An import is written at the top of a program and, in R6RS, inside a
@code{(library ...)}; both are declarations, and nothing else is: an
@code{import} inside a body is a call to somebody's procedure, not a claim
about what the file uses."
  (let* ((top (filter node-data? ast))
         (library-imports
          (append-map (lambda (form)
                        (filter (lambda (clause) (list-headed-by? clause 'import))
                                (form-arguments form)))
                      (filter (lambda (node)
                                (memq (and (node-list? node) (node-head-name node))
                                      library-form-heads))
                              top))))
    (append (filter (lambda (node) (list-headed-by? node 'import)) top)
            library-imports)))

;;; --- where a file says what it imports ---------------------------------
;;;
;;; Two shapes, and only two.  An `(import ...)' form holds specs, and a
;;; `#:use-module' clause of a `define-module' holds one: the datum the keyword
;;; introduces.  A clause is not a list -- the keyword is a node of its own --
;;; so a site carries the keyword as well, which is what a finding points at and
;;; what a fix has to remove along with the spec.

(define-record-type <import-site>
  (make-import-site form anchor specs)
  import-site?
  (form import-site-form) ; the import form, or a clause's keyword
  (anchor import-site-anchor) ; the spec a clause introduces, or #f
  (specs import-site-specs)) ; the specs the site holds

(define (site-clause? site)
  "Return @code{#t} when @var{site} is a `define-module' clause."
  (and (import-site? site) (import-site-anchor site) #t))

;; The clauses of a `define-module' that name another module.
(define module-import-keywords
  '(use-module autoload use-syntax))

(define (module-import-sites ast)
  "Return the import clauses of every `define-module' in @var{ast}."
  (append-map
   (lambda (form)
     (filter-map
      (lambda (clause)
        (and (memq (atom-symbol (car clause)) module-import-keywords)
          (make-import-site (car clause) (cdr clause) (list (cdr clause)))))
      (declaration-clauses form)))
   (filter (lambda (node) (list-headed-by? node 'define-module))
           (filter node-data? ast))))

(define (import-sites ast)
  "Return every place @var{ast} says what it imports, in source order.

An `(import ...)' form, wherever it is written -- at the top of a program or
inside an R6RS `library' -- and a `#:use-module' clause of a Guile module."
  (append (map (lambda (form)
                 (make-import-site form #f (import-specs form)))
               (import-forms ast))
          (module-import-sites ast)))

(define (import-specs form)
  "Return the import specs of @var{form}, which is an @code{import} form."
  (if (list-headed-by? form 'import) (form-arguments form) '()))

(define import-wrappers '(only except prefix rename))

(define (spec-kind spec)
  "Return what kind of import spec @var{spec} is: a wrapper or a library."
  (let ((head (and (node-list? spec) (node-head-name spec))))
    (cond
      [(memq head import-wrappers) head]
      [(and head (node-atom? (node-head spec))) 'library]
      [else #f])))

(define (spec-library-node spec)
  "Return the node that names the library @var{spec} imports, or @code{#f}.

The four wrappers all take the library as their first argument, so the answer
is the same four times: `(only (scheme base) car)' imports `(scheme base)'."
  (case [spec-kind spec]
    [(library) spec]
    [(only except prefix rename) (car (form-arguments spec))]
    [else #f]))

(define (spec-library spec)
  "Return the library @var{spec} imports, as a list of names, or @code{#f}."
  (let ((node (spec-library-node spec)))
    (and node (map element-name (data-elements node)))))

(define (spec-name-nodes spec)
  "Return the atoms an @code{only} spec names, or @code{'()}."
  (if (eq? (spec-kind spec) 'only)
    (filter node-atom? (cddr (data-elements spec)))
    '()))

(define (rename-pair-nodes spec)
  "Return the @code{(old new)} pairs a @code{rename} spec writes."
  (if (eq? (spec-kind spec) 'rename)
    (filter node-list? (cddr (data-elements spec)))
    '()))

(define (nth-or lst k)
  (if (and (>= k 0) (< k (length lst))) (list-ref lst k) #f))

;;; ===== Libraries a file declares =====

;;; A "library" here is whatever a file uses to name itself and say what it
;;; exports: R7RS's `define-library', R6RS's `library', and Guile's
;;; `define-module', which is the one most Scheme in the wild is written in.
(define library-form-heads '(define-library library define-module))

(define (library-forms ast)
  "Return the library forms of @var{ast}: R7RS, R6RS and Guile alike."
  (filter (lambda (node)
            (memq (and (node-list? node) (node-head-name node))
                  library-form-heads))
          (filter node-data? ast)))

(define (library-name form)
  "Return the name a library @var{form} declares, as a list of names."
  (let ((spec (car (form-arguments form))))
    (if (and spec (node-list? spec))
      (map element-name (data-elements spec))
      '())))

(define (export-names-of-nodes nodes)
  "Return the names @var{nodes} exports.

`(export a (rename internal external))' exports `a' and `external': the name a
user of the library writes is the one that counts, which is the second half of
a `rename'."
  (append-map
   (lambda (element)
     (cond
       [(node-atom? element) (let ((name (element-name element))) (if name (list name) '()))]
       [(and (node-list? element) (eq? (node-head-name element) 'rename))
        (let ((external (nth-or (data-elements element) 2)))
          (let ((name (and external (element-name external))))
            (if name (list name) '())))]
       [else '()]))
   nodes))

(define (export-clause-names clause)
  "Return the names one @code{(export ...)} clause exports."
  (export-names-of-nodes (form-arguments clause)))

(define (library-exports form)
  "Return the names @var{form} exports, as the library itself writes them.

R7RS and R6RS write an `(export ...)' clause; Guile writes `#:export (a b)'
and `#:re-export (c d)', which are clauses too, just not lists.  A name a
library re-exports is on its surface all the same: a file that imports the
library can name it."
  (if (list-headed-by? form 'define-module)
    (module-exports form)
    (append-map (lambda (clause)
                  (if (and (node-list? clause)
                        (memq (node-head-name clause) '(export re-export)))
                    (export-clause-names clause)
                    '()))
                (form-arguments form))))

(define (library-key name)
  "Return the key a library @var{name} is looked up by."
  (string-join (map (lambda (part) (or part "")) name) " "))

(define (declared-libraries ast)
  "Return the libraries @var{ast} declares, as @code{(key . exports)} pairs."
  (map (lambda (form) (cons (library-key (library-name form))
                            (library-exports form)))
       (library-forms ast)))

;;; ===== Libraries the project declares =====

(define scan-extensions '(".scm" ".ss" ".sld" ".sls" ".sps"))
(define scan-depth-limit 5)
(define scan-file-limit 400)
(define scan-byte-limit 300000)

(define scan-cache (make-hash-table))

(define (library-file? path)
  (any (lambda (extension) (string-suffix? extension path)) scan-extensions))

(define (directory? path)
  (eq? 'directory (false-if-exception (stat:type (stat path)))))

(define (libraries-under directory)
  "Return every library declared under @var{directory}, bounded and sorted.

The walk is breadth-first with a depth and a file count, and it does not
follow a directory whose name begins with a dot: a project's own sources are
what the question is about, and `.git' alone would answer it more slowly."
  (define (walk pending depth found count)
    (if (or (null? pending) (>= count scan-file-limit) (> depth scan-depth-limit))
      found
      (let ((here (car pending))
            (deeper '())
            (libraries '())
            (examined 0))
        (for-each
         (lambda (name)
           (unless (or (string-prefix? "." name)
                     (member name '("node_modules" "build" "dist" "target" "vendor")))
             (let ((path (string-append here "/" name)))
               (cond
                 [(directory? path) (set! deeper (cons path deeper))]
                 [(library-file? path)
                  (set! examined (+ examined 1))
                  (set! libraries (append (libraries-in-file path) libraries))]))))
         (or (false-if-exception (scandir here)) '()))
        (walk (append (reverse deeper) (cdr pending))
              (+ depth 1)
              (append libraries found)
              (+ count examined)))))
  (walk (list directory) 0 '() 0))

(define (libraries-in-file path)
  "Return the libraries the file at @var{path} declares, if it can be read."
  (let ((text (false-if-exception (call-with-input-file path get-string-all))))
    (if (and (string? text) (< (string-length text) scan-byte-limit)
          ;; a cheap question first: a file that never says either word has no
          ;; library to declare, and reading every file of a tree to find that
          ;; out would make a lint quadratic in the project
          (or (string-contains text "library")
            (string-contains text "define-module")))
      (call-with-values
        (lambda () (read-source #:source text #:dialect 'scheme #:file path))
        (lambda (ast errors) (declared-libraries ast)))
      '())))

(define (project-libraries directory)
  "Return the libraries declared under @var{directory}, cached per directory.

The same directory is scanned once per process, because a lint run asks the
same question about every file in it and the answer cannot change while the
run is going on."
  (if (not directory)
    '()
    (or (hash-ref scan-cache directory)
      (let ((found (libraries-under directory)))
        (hash-set! scan-cache directory found)
        found))))

;;; ===== What a file mentions =====

(define (quoted-node? node)
  (and (node-prefix? node)
    (let ((token (node-token node)))
      (and (string? token) (member token '("'" "`" ",@" ",")) #t))))

(define (referenced-names ast)
  "Return a hash of every name @var{ast} mentions outside its imports.

An import form names the very names it imports, so counting there would call
every import used; a quote is data rather than a mention, so it is not
entered.  A datum comment is code the reader was told not to read."
  (let ((names (make-hash-table)))
    (define (walk node)
      (cond
        [(node-datum-comment? node) #f]
        [(quoted-node? node) #f]
        [(list-headed-by? node 'import) #f]
        [(node-atom? node)
         (let ((name (element-name node)))
           (when (and name (eq? (node-atom-type node) 'symbol))
             (hash-set! names name #t)))]
        [(node-list? node) (for-each walk (node-children node))]
        [(node-conditional? node)
         (let ((feature (node-feature node))
               (guarded (node-guarded-form node)))
           (when feature (walk feature))
           (when guarded (walk guarded)))]))
    (for-each (lambda (node) (when (node-data? node) (walk node))) ast)
    names))

(define (used? names name)
  (and name (hash-ref names name) #t))

(define (mentions ast)
  "Return a hash from every symbol @var{ast} names to how many times it does.

The plainer cousin of `referenced-names`, and the difference is the whole
point of it: `referenced-names` asks what the *program* uses, so it skips a
quote and skips the import forms.  This asks whether a name appears at all,
because the question it answers is whether a definition is dead -- and a name
under a quote is a mention a formatter cannot rule out as a use."
  (let ((counts (make-hash-table)))
    (for-each-node
     (lambda (node)
       (when (and (node-atom? node) (eq? (node-atom-type node) 'symbol))
         (let ((name (node-value node)))
           (hash-set! counts name (+ 1 (or (hash-ref counts name) 0))))))
     ast)
    counts))

;;; ===== Where a fix may cut =====

(define (deletion-range elements node)
  "Return the @code{(start . end)} a fix deletes to remove @var{node}.

The whitespace *before* the element goes with it, which is what keeps
`(only (scheme base) car cdr)' reading as `(only (scheme base) cdr)' when the
first name is removed rather than as a form with a double space in it."
  (and elements
    (let loop ((rest elements) (previous #f))
      (cond
        [(null? rest) #f]
        [(eq? (car rest) node)
         ;; the first element has nothing before it, so it takes the space
         ;; *after* it instead -- otherwise removing it leaves a double space
         (if previous
           (cons (source-location-index (node-end previous))
                 (source-location-index (node-end node)))
           (cons (source-location-index (node-start node))
                 (if (pair? (cdr rest))
                   (source-location-index (node-start (cadr rest)))
                   (source-location-index (node-end node)))))]
        [else (loop (cdr rest) (car rest))]))))

;;; ===== Libraries sift knows without seeing them =====

;; The namespaces whose exports sift cannot read and whose absence from the
;; project says nothing: a standard library, a Scheme implementation's own, and
;; the SRFI collection.  A library outside these and outside the project is
;; what `library-not-found' reports.
(define well-known-namespaces
  '("scheme" "srfi" "srfi-1" "rnrs" "r6rs" "r7rs" "guile" "ice-9" "gnu"
    "chibi" "chicken" "racket" "gambit" "kawa" "loko" "sagittarius" "mit"
    "chez" "larceny" "slib" "sld" "spock"))

(define (well-known-library? name)
  "Return @code{#t} when @var{name} is a library sift cannot be sure about."
  (and (pair? name)
    (let ((first (car name)))
      (or (member first well-known-namespaces)
        ;; `(scheme base)' names its namespace twice; a single-component name
        ;; such as `(srfi-1)' is a namespace too.
        (any (lambda (part) (member part well-known-namespaces)) name)
        (and (string-prefix? "srfi" first) #t)))))
