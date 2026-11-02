;; sift --- Guix checkers
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Guix is Scheme with three things added: gexps -- the `#~' family of reader
;; prefixes -- a package record whose fields are a vocabulary of their own, and
;; a build stratum that a phase lambda runs in rather than the one the file
;; runs in.  Most of what goes wrong in a package definition goes wrong at one
;; of those three seams, and every rule here reads it off the text.
;;
;; The gexp rules are about the seam itself: a gexp that is quoted is the form
;; and not the object, an `#$' with no gexp around it is a token with no
;; meaning, a `#$output` in a file-making gexp is a `getenv` that answers
;; false, and a build module imported without `with-imported-modules' is a
;; module the builder will not find.  The `assoc-ref' and `string-append' rules
;; are the same seam seen from the older spelling: what the gexp machinery
;; substitutes, the alists made the build compute by hand.
;;
;; The package rules are the manual's own: a synopsis is a lower-case phrase
;; without a period, a description is a literal string because xgettext has to
;; be able to read it, a hash is 52 base32 characters, and an input list needs
;; no labels.

(define-module (sift checkers guix)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (sift records)
  #:use-module (sift check base)
  #:export (guix-checkers
            gexp-quoted-checker
            ungexp-outside-gexp-checker
            gexp-output-script-checker
            assoc-ref-outputs-checker
            string-append-file-append-checker
            phase-name-quoting-checker
            use-modules-in-gexp-checker
            package-synopsis-checker
            package-description-literal-checker
            sha256-base32-checker
            inputs-label-alist-checker))

;;; ===== Reading a gexp =====

(define (gexp-form? node)
  "Return @code{#t} when @var{node} opens a gexp: `#~x` or `(gexp x)`."
  (or (and (node-prefix? node) (equal? (node-token node) "#~"))
    (list-headed-by? node 'gexp)))

(define (gexp-ancestor? ancestors)
  "Return @code{#t} when one of @var{ancestors} opens a gexp."
  (any gexp-form? ancestors))

(define (ungexp-node? node)
  "Return @code{#t} when @var{node} is `#$`, `#$@`, `#+` or `#+@`."
  (and (node-prefix? node)
    (member (node-token node) '("#$" "#$@" "#+" "#+@"))))

(define (quoted-ancestor? ancestors)
  "Return the quotation that encloses a node, or @code{#f}."
  (find (lambda (ancestor)
          (or (and (node-prefix? ancestor)
                (member (node-token ancestor) '("'" "`")))
            (and (node-list? ancestor)
              (memq (node-head-name ancestor) '(quote quasiquote)))))
        ancestors))

(define (inside-any? ancestors names)
  "Return the nearest ancestor headed by one of @var{names}, or @code{#f}."
  (find (lambda (ancestor)
          (and (node-list? ancestor)
            (memq (node-head-name ancestor) names)))
        ancestors))

(define derivation-forms
  '(gexp->derivation gexp->script gexp->file derivation computed-file))

(define (quasiquote-form? node)
  "Return @code{#t} when @var{node} is a quasiquote."
  (or (and (node-prefix? node) (equal? (node-token node) "`"))
    (list-headed-by? node 'quasiquote)))

(define (build-context? ancestors)
  "Return @code{#t} inside a gexp, or inside a build body a derivation takes.

Package code written before gexps puts the builder in a quasiquote passed to
`gexp->derivation`, where `,' plays the part `#$` plays in a gexp and
`%outputs`/`%build-inputs` are what the builder reads.  A rule about a build
body asks this instead of `gexp-ancestor?' so that both spellings are read as
the same thing; the rules that are about gexp syntax itself keep asking
`gexp-ancestor?'."
  (let loop ((rest ancestors))
    (cond
      [(null? rest) #f]
      [(gexp-form? (car rest)) #t]
      [(quasiquote-form? (car rest))
       (let ((parent (find node-list? (cdr rest))))
         (and parent (memq (node-head-name parent) derivation-forms) #t))]
      [else (loop (cdr rest))])))

(define (string-literal-text node)
  "Return the text of the string @var{node} without its quotes, or @code{#f}."
  (and (string-atom? node)
    (let ((value (node-value node)))
      (and (>= (string-length value) 2)
        (substring value 1 (- (string-length value) 1))))))

;;; ===== gexp-quoted =====
;;;
;;; `'#~(...)' is a quoted gexp: what is quoted is the *form*, so what is
;;; passed to the build system is a list beginning with `gexp' rather than a
;;; gexp object.  It is the single most common way to get a build that does
;;; nothing, and it is invisible in a diff.

(define (gexp-quoted-warnings ast source dialect)
  (let ((warnings '()))
    (walk-with-ancestors
     (lambda (node ancestors)
       (when (and (gexp-form? node) (quoted-ancestor? ancestors))
         (set! warnings
               (cons (make-lint-warning
                      'gexp-quoted 'warning
                      "a quoted gexp is the form and not the object; write the gexp where it is used"
                      (node-start node))
                     warnings))))
     ast)
    (reverse warnings)))

;;; ===== ungexp-outside-gexp =====
;;;
;;; `#$' is only meaningful inside a gexp, where the reader has been told a
;;; gexp is being read.  Anywhere else it is a token whose meaning belongs to
;;; no form -- usually a gexp that lost its `#~'.

(define (ungexp-outside-gexp-warnings ast source dialect)
  (let ((warnings '()))
    (walk-with-ancestors
     (lambda (node ancestors)
       (when (and (ungexp-node? node) (not (gexp-ancestor? ancestors)))
         (set! warnings
               (cons (make-lint-warning
                      'ungexp-outside-gexp 'warning
                      (format #f "`~a` is only read inside a gexp; this one has no `#~~` around it"
                              (node-token node))
                      (node-start node))
                     warnings))))
     ast)
    (reverse warnings)))

;;; ===== gexp-output-script =====
;;;
;;; `#$output` is the directory of the derivation's output.  A `program-file',
;;; `scheme-file', `gexp->script' or `gexp->file' produces a store file, not a
;;; derivation with an `out', so the builder reads `getenv "out"' and gets
;;; false -- which the manual says outright: `#$output` "only makes sense for
;;; gexps passed to `gexp->derivation'".

(define script-file-forms
  '(program-file scheme-file gexp->script gexp->file))

(define (output-reference? node)
  (and (node-prefix? node) (equal? (node-token node) "#$")
    (let ((child (node-child node)))
      (and child (symbol-atom? child 'output)))))

(define (gexp-output-script-warnings ast source dialect)
  (let ((warnings '()))
    (walk-with-ancestors
     (lambda (node ancestors)
       (when (and (output-reference? node)
               (inside-any? ancestors script-file-forms))
         (set! warnings
               (cons (make-lint-warning
                      'gexp-output-script 'warning
                      "a file-making gexp has no output directory; `#$output` is for a derivation"
                      (node-start node))
                     warnings))))
     ast)
    (reverse warnings)))

;;; ===== assoc-ref-outputs =====
;;;
;;; `(assoc-ref %outputs "out")' is how a gexp-less builder named its output.
;;; Inside a gexp the name is already there, and `#$output` is what the manual
;;; writes everywhere it shows a build.

(define (assoc-ref-parts node)
  "Return @code{(container . name)} for an `assoc-ref` call, or @code{#f}."
  (and (list-headed-by? node 'assoc-ref)
    (= 2 (argument-count node))
    (let ((arguments (form-arguments node)))
      (and (node-atom? (car arguments))
        (string-atom? (cadr arguments))
        (cons (node-value (car arguments)) (node-value (cadr arguments)))))))

(define (assoc-ref-outputs-warnings ast source dialect)
  (let ((warnings '()))
    (walk-with-ancestors
     (lambda (node ancestors)
       (when (and (list-headed-by? node 'assoc-ref)
               (build-context? ancestors))
         (let* ((parts (assoc-ref-parts node))
                (gexp? (gexp-ancestor? ancestors)))
           (when (and parts (member (car parts) '("%outputs" "outputs")))
             (set! warnings
                   (cons (make-lint-warning/range
                          'assoc-ref-outputs 'warning
                          (if gexp?
                            "a gexp names an output with `#$output`; the alist is what the form was for"
                            "a build body names its output with `%output`; the alist is what the form was for")
                          (node-start node)
                          (node-index node) (node-end-index node)
                          (lambda (text start end)
                            (if gexp? "#$output" "%output")))
                         warnings))))))
     ast)
    (reverse warnings)))

;;; ===== string-append-file-append =====
;;;
;;; The manual draws the difference: `#$coreutils' inserted into a
;;; `string-append' builds the file name at run time, while
;;; `#$(file-append coreutils "/bin/ls")' puts the whole absolute file name in
;;; the script, where `grep' and `the-store' can find it.

(define (file-append-candidate node)
  "Return @code{(package . suffixes)} when @var{node} is a gexp string-append."
  (and (list-headed-by? node 'string-append)
    (>= (argument-count node) 2)
    (let* ((arguments (form-arguments node))
           (first-node (car arguments))
           (suffixes (cdr arguments)))
      (and (node-prefix? first-node)
        ;; `#$' in a gexp and `,' in a quasiquoted build body are the same
        ;; ungexp, and the rule only runs inside one of the two
        (member (node-token first-node) '("#$" ","))
        (let ((package (node-child first-node)))
          (and package (node-atom? package)
            (not (symbol-atom? package 'output))
            (every string-atom? suffixes)
            (cons package suffixes)))))))

(define (string-append-file-append-warnings ast source dialect)
  (let ((warnings '()))
    (walk-with-ancestors
     (lambda (node ancestors)
       (when (build-context? ancestors)
         (let ((candidate (file-append-candidate node)))
           (when candidate
             (let* ((package (car candidate))
                    (suffixes (cdr candidate))
                    (gexp? (gexp-ancestor? ancestors)))
               (set! warnings
                     (cons (make-lint-warning/range
                            'string-append-file-append 'info
                            (if gexp?
                              "`file-append` puts the whole file name in the store path, where a run-time `string-append` does not"
                              "a quasiquoted build body builds this file name at run time; a gexp says it with `#$(file-append ...)` elsewhere")
                            (node-start node)
                            (node-index node) (node-end-index node)
                            (and gexp?
                              (lambda (text start end)
                                (format #f "#$(file-append ~a ~a)"
                                        (node-source source package)
                                        (string-join
                                         (map (lambda (suffix)
                                                (node-source source suffix))
                                              suffixes)
                                         " ")))))
                           warnings)))))))
     ast)
    (reverse warnings)))

;;; ===== phase-name-quoting =====
;;;
;;; A phase is named by a symbol, and `(delete configure)' deletes the value of
;;; a variable called `configure' -- which is not what a package means.  The
;;; manual's `.dir-locals.el' indents `delete', `replace', `add-before' and
;;; `add-after' as phrases of `modify-phases', which is the shape this checks.

(define phase-name-forms
  '(delete replace add-before add-after))

(define (phase-name-count node)
  "Return how many leading arguments of @var{node} name phases."
  (case [node-head-name node]
    [(delete replace) 1]
    [(add-before add-after) 2]
    [else 0]))

(define (quoted-name? node)
  (or (and (node-prefix? node) (equal? (node-token node) "'")
        (let ((child (node-child node))) (and child (node-atom? child))))
    (and (list-headed-by? node 'quote)
      (let ((arguments (form-arguments node)))
        (and (= 1 (length arguments)) (node-atom? (car arguments)))))))

(define (phase-name-text node)
  "Return the phase @var{node} names, or @code{#f}."
  (let ((text (cond
                [(and (node-atom? node) (eq? (node-atom-type node) 'symbol))
                 (node-value node)]
                [(string-atom? node) (string-literal-text node)]
                [(node-prefix? node) (and (node-child node) (phase-name-text (node-child node)))]
                [else #f])))
    (and text (not (string-null? text)) text)))

(define (phase-name-quoting-warnings ast source dialect)
  (let ((warnings '()))
    (walk-with-ancestors
     (lambda (node ancestors)
       (when (and (node-list? node)
               (memq (node-head-name node) phase-name-forms)
               (inside-any? ancestors '(modify-phases)))
         (let ((count (phase-name-count node))
               (arguments (form-arguments node)))
           (for-each
            (lambda (argument)
              (unless (quoted-name? argument)
                (let ((name (phase-name-text argument)))
                  (when name
                    (set! warnings
                          (cons (make-lint-warning/range
                                 'phase-name-quoting 'warning
                                 (format #f "`~a` names a phase, and a name is a quoted symbol" name)
                                 (node-start argument)
                                 (node-index argument) (node-end-index argument)
                                 (lambda (text start end) (format #f "'~a" name)))
                                warnings))))))
            (take arguments (min count (length arguments)))))))
     ast)
    (reverse warnings)))

;;; ===== use-modules-in-gexp =====
;;;
;;; A `use-modules' inside a gexp is read in the builder, where only the
;;; modules the gexp was marked with exist.  `with-imported-modules' is that
;;; marking, and it has lexical scope: the gexp has to be written inside it,
;;; not returned from a procedure called inside it.

(define (build-library? node)
  "Return @code{#t} when @var{node} names a module of the build stratum."
  (and (node-list? node)
    (let ((head (node-head node)))
      (and head (node-atom? head)
        (member (node-value head) '("guix" "gnu"))))))

(define (build-modules-given? ancestors)
  "Return @code{#t} when the enclosing derivation is told its modules."
  (let ((form (find (lambda (ancestor)
                      (and (node-list? ancestor)
                        (memq (node-head-name ancestor) derivation-forms)))
                    ancestors)))
    (and form
      (any (lambda (argument)
             (and (node-atom? argument)
               (member (node-value argument) '("#:modules" "#:extensions"))))
           (form-arguments form)))))

(define (use-modules-in-gexp-warnings ast source dialect)
  (let ((warnings '()))
    (walk-with-ancestors
     (lambda (node ancestors)
       (when (and (list-headed-by? node 'use-modules)
               (build-context? ancestors) (not (inside-any? ancestors '(with-imported-modules with-extensions)))
               (not (build-modules-given? ancestors)))
         (when (any build-library? (form-arguments node))
           (set! warnings
                 (cons (make-lint-warning
                        'use-modules-in-gexp 'warning
                        (if (gexp-ancestor? ancestors)
                          "this build module is imported inside a gexp, where it exists only if `with-imported-modules` marks the gexp"
                          "a quasiquoted build body imports a build module the builder only has through `#:modules`")
                        (node-start node))
                       warnings)))))
     ast)
    (reverse warnings)))

;;; ===== The package record =====

(define (package-field-values ast field)
  "Return the value nodes of @var{field} in every `package` form of @var{ast}."
  (let ((values '()))
    (for-each-form
     (lambda (node)
       (when (list-headed-by? node 'package)
         (for-each
          (lambda (candidate)
            (when (and (node-list? candidate)
                    (eq? (atom-symbol (node-head candidate)) field))
              (let ((arguments (form-arguments candidate)))
                (when (pair? arguments)
                  (set! values (cons (car arguments) values))))))
          (form-arguments node))))
     ast)
    (reverse values)))

;;; ===== package-synopsis =====
;;;
;;; The manual: "Synopses must start with a capital letter and must not end
;;; with a period.  They must not start with 'a' or 'the', which usually does
;;; not bring anything".

(define synopsis-articles
  '("a " "an " "the "))

(define (package-synopsis-warnings ast source dialect)
  (let ((warnings '()))
    (define (report node message)
      (set! warnings
            (cons (make-lint-warning 'package-synopsis 'warning message
                                     (node-start node))
                  warnings)))
    (for-each
     (lambda (node)
       (let ((text (string-literal-text node)))
         (when text
           (let ((lower (string-downcase text)))
             (when (any (lambda (article) (string-prefix? article lower))
                        synopsis-articles)
               (report node "a synopsis that opens with an article says less; start with the thing itself"))
             (when (string-suffix? "." text)
               (report node "the manual asks a synopsis not to end with a period"))
             (let ((initial (and (pair? (string->list text))
                              (car (string->list text)))))
               (when (and initial (char-lower-case? initial))
                 (report node "a synopsis starts with a capital letter")))))))
     (package-field-values ast 'synopsis))
    (reverse warnings)))

;;; ===== package-description-literal =====
;;;
;;; "To allow xgettext to extract them as translatable strings, synopses and
;;; descriptions must be literal strings.  This means that you cannot use
;;; `string-append` or `format` to construct these strings".

(define (package-description-literal-warnings ast source dialect)
  (let ((warnings '()))
    (for-each
     (lambda (node)
       (unless (string-atom? node)
         (set! warnings
               (cons (make-lint-warning
                      'package-description-literal 'warning
                      "xgettext can only extract a description written as one literal string"
                      (node-start node))
                     warnings))))
     (package-field-values ast 'description))
    (reverse warnings)))

;;; ===== sha256-base32 =====
;;;
;;; "The `sha256` field specifies the expected SHA256 hash of the file being
;;; downloaded.  It is mandatory", and the hash is written `(base32 "...")':
;;; 32 bytes in base32 is 52 lower-case characters, and a hash that is not that
;;; long cannot be the one the download has.

(define (base32-text node)
  (and (list-headed-by? node 'base32)
    (let ((arguments (form-arguments node)))
      (and (= 1 (length arguments))
        (string-literal-text (car arguments))))))

(define (base32-hash? text)
  (and (= 52 (string-length text))
    (every (lambda (character)
             (or (char-numeric? character)
               (and (char-alphabetic? character) (char-lower-case? character))))
           (string->list text))))

(define (sha256-base32-warnings ast source dialect)
  (let ((warnings '()))
    (walk-with-ancestors
     (lambda (node ancestors)
       (when (and (list-headed-by? node 'sha256) (inside-any? ancestors '(origin)))
         (let ((arguments (form-arguments node)))
           (when (pair? arguments)
             (let* ((value (car arguments))
                    (text (and value (base32-text value))))
               (cond
                 [(not (list-headed-by? value 'base32))
                  (set! warnings
                        (cons (make-lint-warning
                               'sha256-base32 'warning
                               "a `sha256` is written `(base32 \"...\")`, the encoding Guix stores a hash in"
                               (node-start value))
                              warnings))]
                 [(not (and text (base32-hash? text)))
                  (set! warnings
                        (cons (make-lint-warning
                               'sha256-base32 'warning
                               "a SHA-256 in base32 is 52 lower-case characters"
                               (node-start value))
                              warnings))]))))))
     ast)
    (reverse warnings)))

;;; ===== inputs-label-alist =====
;;;
;;; An input list is a list of packages.  The ``(("gawk" ,gawk))'' spelling is
;;; the pre-gexp one the manual used to explain labels: it names each package
;;; twice, and the label is a string a build phase has to be told to look up.

(define (quasiquoted-datum node)
  (cond
    [(and (node-prefix? node) (equal? (node-token node) "`")) (node-child node)]
    [(list-headed-by? node 'quasiquote)
     (let ((arguments (form-arguments node)))
       (and (= 1 (length arguments)) (car arguments)))]
    [else #f]))

(define (unquoted-value node)
  (cond
    [(and (node-prefix? node) (equal? (node-token node) ",")) (node-child node)]
    [(list-headed-by? node 'unquote)
     (let ((arguments (form-arguments node)))
       (and (= 1 (length arguments)) (car arguments)))]
    [else #f]))

(define (input-entry node)
  "Return the package expression of a @code{(\"label\" ,package)} entry, or @code{#f}."
  (let ((elements (and (node-list? node) (node-elements node))))
    (and elements (= 2 (length elements))
      (string-atom? (car elements))
      (unquoted-value (cadr elements)))))

(define (inputs-label-alist-warnings ast source dialect)
  (let ((warnings '()))
    (walk-with-ancestors
     (lambda (node ancestors)
       (when (and (node-list? node)
               (memq (node-head-name node) '(inputs native-inputs propagated-inputs))
               (inside-any? ancestors '(package)))
         (let ((arguments (form-arguments node)))
           (when (= 1 (length arguments))
             (let* ((datum (quasiquoted-datum (car arguments)))
                    (entries (and datum (node-list? datum) (node-elements datum)))
                    (packages (and entries (map input-entry entries))))
               (when (and entries (every (lambda (package) package) packages))
                 (set! warnings
                       (cons (make-lint-warning/range
                              'inputs-label-alist 'warning
                              "an input list is a list of packages; the labels are the pre-gexp spelling and say each name twice"
                              (node-start node)
                              (node-index node) (node-end-index node)
                              (lambda (text start end)
                                (format #f "(~a (list ~a))"
                                        (node-head-name node)
                                        (string-join
                                         (map (lambda (package)
                                                (node-source source package))
                                              packages)
                                         " "))))
                             warnings))))))))
     ast)
    (reverse warnings)))

;;; ===== The checkers =====

(define gexp-quoted-checker
  (checker 'gexp-quoted
           "A gexp written under a quote, where what is quoted is the form"
           '(scheme)
           gexp-quoted-warnings
           #:severity 'warning
           #:example '((input . "'#~(mkdir #$output)")
                       (fix . "#~(mkdir #$output)"))))

(define ungexp-outside-gexp-checker
  (checker 'ungexp-outside-gexp
           "An `#$`, `#$@`, `#+` or `#+@` with no gexp around it"
           '(scheme)
           ungexp-outside-gexp-warnings
           #:severity 'warning
           #:example '((input . "(list #$output)")
                       (fix . "(list #$output)"))))

(define gexp-output-script-checker
  (checker 'gexp-output-script
           "A `#$output` in a gexp that makes a file rather than a derivation"
           '(scheme)
           gexp-output-script-warnings
           #:severity 'warning
           #:example '((input . "(program-file \"run.sh\" #~(execl #$output \"x\" #f))")
                       (fix . "(program-file \"run.sh\" #~(execl #$(file-append coreutils \"/bin/ls\") \"ls\" #f))"))))

(define assoc-ref-outputs-checker
  (checker 'assoc-ref-outputs
           "`(assoc-ref %outputs \"out\")` inside a gexp, where `#$output` says it"
           '(scheme)
           assoc-ref-outputs-warnings
           #:severity 'warning
           #:example '((input . "#~(mkdir (assoc-ref %outputs \"out\"))")
                       (fix . "#~(mkdir #$output)"))))

(define string-append-file-append-checker
  (checker 'string-append-file-append
           "A gexp `string-append` of a package and a literal path, where `file-append` says it"
           '(scheme)
           string-append-file-append-warnings
           #:severity 'info
           #:example '((input . "#~(system* (string-append #$coreutils \"/bin/ls\"))")
                       (fix . "#~(system* #$(file-append coreutils \"/bin/ls\"))"))))

(define phase-name-quoting-checker
  (checker 'phase-name-quoting
           "A phase named by an unquoted symbol or a string in `modify-phases`"
           '(scheme)
           phase-name-quoting-warnings
           #:severity 'warning
           #:example '((input . "(modify-phases %standard-phases (delete configure))")
                       (fix . "(modify-phases %standard-phases (delete 'configure))"))))

(define use-modules-in-gexp-checker
  (checker 'use-modules-in-gexp
           "A build module imported inside a gexp that `with-imported-modules` does not mark"
           '(scheme)
           use-modules-in-gexp-warnings
           #:severity 'warning
           #:example '((input . "#~(use-modules (guix build utils))")
                       (fix . "(with-imported-modules '((guix build utils))\n  #~(use-modules (guix build utils)))"))))

(define package-synopsis-checker
  (checker 'package-synopsis
           "A synopsis that opens with an article, ends with a period, or starts in lower case"
           '(scheme)
           package-synopsis-warnings
           #:severity 'warning
           #:example '((input . "(package (synopsis \"A file frobber.\"))")
                       (fix . "(package (synopsis \"File frobber\"))"))))

(define package-description-literal-checker
  (checker 'package-description-literal
           "A description built by `string-append` rather than written as one string"
           '(scheme)
           package-description-literal-warnings
           #:severity 'warning
           #:example '((input . "(package (description (string-append \"Frob \" \"files.\")))")
                       (fix . "(package (description \"Frob files.\"))"))))

(define sha256-base32-checker
  (checker 'sha256-base32
           "A `sha256` that is not `(base32 \"...\")` of 52 lower-case characters"
           '(scheme)
           sha256-base32-warnings
           #:severity 'warning
           #:example '((input . "(origin (sha256 (base32 \"abc\")))")
                       (fix . "(origin (sha256 (base32 \"0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i\")))"))))

(define inputs-label-alist-checker
  (checker 'inputs-label-alist
           "An `inputs` written as a labelled alist instead of a list of packages"
           '(scheme)
           inputs-label-alist-warnings
           #:severity 'warning
           #:example '((input . "(package (inputs `((\"gawk\" ,gawk))))")
                       (fix . "(package (inputs (list gawk)))"))))

;;; ===== The list =====

(define guix-checkers
  (list gexp-quoted-checker
        ungexp-outside-gexp-checker
        gexp-output-script-checker
        assoc-ref-outputs-checker
        string-append-file-append-checker
        phase-name-quoting-checker
        use-modules-in-gexp-checker
        package-synopsis-checker
        package-description-literal-checker
        sha256-base32-checker
        inputs-label-alist-checker))
