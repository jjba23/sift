;; sift --- the browser facade's tests
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `(sift web)' is one procedure of one JSON string to one JSON string, and
;; that is deliberate: it is the whole of what the browser build can do, so it
;; is the whole of what can be tested without a browser.  These tests drive it
;; under Guile, in the same process, and assert the answers -- the formatted
;; text, the findings, the fixes, and the answers to requests that should be
;; refused.
;;
;; The point of testing the facade rather than the editor is that the editor is
;; downstream of it: if `{"op":"lint"}' answers what the command line answers,
;; then a bug in the page is a bug in a page, and not in sift.
;;
;; The one thing these tests cannot check is the WebAssembly build itself.  That
;; needs Hoot and a Wasm runtime (see web/README.org), and what it would catch
;; is a difference between Guile and Hoot, not a difference between sift and
;; itself.

(define-module (tests web)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift json)
  #:use-module (sift web)
  #:use-module (tests helpers)
  #:export (spec))

;;; ===== Asking, and reading the answer =====

(define (answer request)
  "Return the JSON answer to @var{request}, read back as sift's JSON values."
  (json-read (web-request (json-write request))))

(define (asked . fields)
  "Return a request object of @var{fields}, a list of @code{(name . value)}."
  (apply json-object fields))

(define (result-of request)
  "Return the result of @var{request}, or stop the test with its message."
  (let ((answer (answer request)))
    (if (eq? (json-ref answer "ok") json-true)
      (json-ref answer "result")
      (error "sift refused a request it should have answered"
             (json-ref (json-ref answer "error") "message")))))

(define (field-of object name)
  "Return the member @var{name} of the JSON @var{object}."
  (json-ref object name))

(define (warnings-of request)
  (vector->list (field-of (result-of request) "warnings")))

(define* (assert-refused #:key request kind (name "the request is refused"))
  "Assert that @var{request} comes back as an error of @var{kind}."
  (let* ((answer (answer request))
         (error (json-ref answer "error")))
    (assert-equal #:expect json-false #:got (json-ref answer "ok")
      #:name (format #f "~a: it is refused" name))
    (assert-equal #:expect kind #:got (json-ref error "kind")
      #:name (format #f "~a: as a ~a error" name kind))
    (assert-true (> (string-length (json-ref error "message")) 0)
      #:name (format #f "~a: with a sentence to read" name))))

;;; ===== The seam =====
;;;
;;; Everything above tests what the facade *answers*.  What follows tests what
;;; it is *allowed to be*: `src/sift/' is the library `bin/sift' installs, and
;;; it must not grow a single word about the browser -- no `hoot' feature, no
;;; `wasm', no comment explaining itself in terms of a tab.  A dependency that
;;; runs one way is a dependency a reader can trust, and these two tests are how
;;; the direction is kept rather than remembered.

(define browser-words
  '("hoot" "wasm" "webassembly" "browser"))

(define (browser-word-in text)
  "Return the first word in @var{browser-words} that @var{text} mentions, or #f."
  (let ((lower (string-downcase text)))
    (find (lambda (word) (string-contains lower word)) browser-words)))

(define (core-source-files directory)
  "Return every Scheme file under @var{directory}, recursively, sorted."
  (let loop ((pending (list directory)) (found '()))
    (if (null? pending)
      (sort found string<?)
      (let* ((here (car pending))
             (names (or (scandir here (lambda (name) (not (string-prefix? "." name))))
                      '()))
             (paths (map (lambda (name) (string-append here "/" name)) names)))
        (loop (append (filter (lambda (path) (eq? 'directory (stat:type (stat path))))
                              paths)
                      (cdr pending))
              (append (filter (lambda (path) (string-suffix? ".scm" path)) paths)
                      found))))))

(define (relative-to-root path)
  "Return @var{path} without the checkout's own prefix, for a readable failure."
  (let ((root (string-append (test-root) "/")))
    (if (string-prefix? root path) (substring path (string-length root)) path)))

(define (core-sources)
  "Return the files of the core library -- everything the CLI ships."
  (core-source-files (string-append (test-root) "/src/sift")))

(define (core-modules-used-by-facade)
  "Return the core modules @code{(sift web)} imports, as module names.

`(sift web)' is the whole of the browser's knowledge of sift, so this list is
the whole of the browser build's dependency on the core: five modules, and a
change to it is a change to what a Wasm build has to carry.  The last of them
is `(sift analysis)`, the buffer half of the language server, which is what
lets a tab answer a hover and a jump with the same code an editor beside a
checkout gets."
  (filter (lambda (name) (eq? (car name) 'sift))
          (map module-name (module-uses (resolve-module '(sift web))))))

(define (module-name<? a b)
  (string<? (string-join (map symbol->string a) " ")
            (string-join (map symbol->string b) " ")))

;; The Guile libraries Hoot carries itself, so that a `(ice-9 ...)' import of
;; one of them needs no stand-in here.  Everything else the browser closure
;; imports has to appear in web/compat, which is what the test below says.
(define hoot-shipped-libraries
  '((ice-9 match) (ice-9 rdelim) (ice-9 regex) (ice-9 control) (ice-9 optargs)
    (ice-9 binary-ports) (ice-9 exceptions) (ice-9 i18n) (ice-9 atomic)))

(define (sift-modules-in-closure)
  "Return the names of the sift modules the browser build compiles.

`(sift web)' and everything it reaches through sift: the answer is walked by
name, so that a module reached twice is visited once and the walk never depends
on whether what a module exposes is a module object or an interface.

The walk stops at sift's own modules on purpose.  What `(guile)' imports for its
own implementation is Guile's business, and a list that included it would be
about the host rather than about sift."
  (let loop ((pending '((sift web))) (seen '()))
    (if (null? pending)
      (reverse seen)
      (let ((name (car pending)))
        (if (member name seen)
          (loop (cdr pending) seen)
          (loop (append (filter (lambda (used) (eq? (car used) 'sift))
                                (map module-name
                                     (module-uses (resolve-module name))))
                        (cdr pending))
                (cons name seen)))))))

(define (ice-9-libraries-in-closure)
  "Return the `(ice-9 ...)' libraries the browser build imports, once each.

This is the list web/compat has to answer for: every one of them is either a
library the target ships or a file in web/compat, and the test below fails by
name when a core module starts importing one that is neither."
  (delete-duplicates
   (sort (append-map (lambda (name)
                       (filter (lambda (used) (eq? (car used) 'ice-9))
                               (map module-name
                                    (module-uses (resolve-module name)))))
                     (sift-modules-in-closure))
         module-name<?)
   equal?))

(define (shimmed? name)
  "Return @code{#t} when web/compat carries a stand-in for the library @var{name}."
  (file-exists?
   (string-append (test-root) "/web/compat/"
                  (string-join (map symbol->string name) "/")
                  ".scm")))

;;; ===== The page, and the libraries a build finds =====
;;;
;;; Everything above tests what the facade *answers*.  What follows tests the
;;; three things around it that go wrong silently: a URL that points above the
;;; directory the page is served from, a URL that does not change when the build
;;; does, and a stand-in library that answers something other than the library
;;; it stands in for.

(define (web-file name)
  "Return the path of @var{name} in the browser directory."
  (string-append (test-root) "/web/" name))

(define (attribute-values attribute text)
  "Return every value of @var{attribute} in @var{text}, in the order written."
  (let ((pattern (make-regexp (string-append attribute "=\"([^\"]*)\""))))
    (let loop ((start 0) (found '()))
      (let ((match (regexp-exec pattern text start)))
        (if match
          (loop (match:end match 0) (cons (match:substring match 1) found))
          (reverse found))))))

(define (page-urls)
  "Return every URL the page names: a stylesheet, an icon, a script."
  (let ((text (slurp (web-file "index.html"))))
    (append (attribute-values "src" text) (attribute-values "href" text))))

(define (compat-load-path directory)
  "Return the load path that puts the browser's SRFI-13 in front, as flags.

The browser build puts the whole of `web/compat' there, because every library the
target lacks needs a stand-in.  A test can put one file there: the other stand-ins
are written as R7RS libraries, which is what Hoot reads and Guile's default
language does not, so a Guile process can load the SRFI-13 stand-in and nothing
else of the directory.  That is enough to ask the question, which is whether the
thirteen procedures sift calls answer what Guile's answer."
  (append-map (lambda (where) (list "-L" where))
              (list (string-append directory "/compat")
                    (string-append (test-root) "/src")
                    (string-append (test-root) "/web"))))

(define (stand-in-answers expressions)
  "Return what the stand-in answers for @var{expressions}, one string each.

A process of its own and the same temporary `(srfi srfi-13)', for the reason
`answer-over-compat' needs them: in this one, `(srfi srfi-13)' is already
Guile's.  Each answer is printed with `write', so what comes back is compared
as the text of a value rather than as a value the reader has to know again."
  (with-temp-directory
   (lambda (directory)
     (let* ((compat (string-append directory "/compat/srfi"))
            (script (string-append directory "/ask.scm"))
            (out-file (string-append directory "/answers"))
            (err-file (string-append directory "/answers.err")))
       (make-directories compat)
       (spit (string-append compat "/srfi-13.scm")
             (slurp (web-file "compat/srfi/srfi-13.scm")))
       (spit script
             (format #f "(use-modules (ice-9 format) (srfi srfi-13))~%~a"
                     (string-join
                      (map (lambda (expression)
                             (format #f "(write ~a) (newline)" expression))
                           expressions)
                      "\n")))
       (let* ((line (string-join (append (list "guile")
                                         (compat-load-path directory)
                                         (list "-s" script))
                                 " "))
              (command (format #f "~a > ~a 2> ~a; echo $?" line out-file err-file))
              (status (call-with-input-string
                        (let ((port (open-input-pipe command)))
                          (let ((answer (read-line port)))
                            (close-pipe port)
                            (or answer "1")))
                        read)))
         (when (not (zero? status))
           (error "the stand-in did not load" (string-trim-both (slurp err-file))))
         (string-split (string-trim-both (slurp out-file)) #\newline))))))

(define (answer-over-compat request)
  "Return what @code{(sift web)} answers to @var{request}, over the stand-in.

A process of its own is the only way to ask: in this one, `(srfi srfi-13)' is
already Guile's, and a module is loaded once.  The script and the request are
files in a temporary directory, so nothing here depends on how a shell would
quote a JSON document.

The answer is compared with this process's, so what the test asserts is that the
stand-in and the library agree -- and a stand-in that answers differently is
exactly the bug that would reach a tab and no test."
  (with-temp-directory
   (lambda (directory)
     (let* ((compat (string-append directory "/compat/srfi"))
            (request-file (string-append directory "/request.json"))
            (script (string-append directory "/ask.scm"))
            (out-file (string-append directory "/answer.json"))
            (err-file (string-append directory "/answer.err")))
       (make-directories compat)
       (spit (string-append compat "/srfi-13.scm")
             (slurp (web-file "compat/srfi/srfi-13.scm")))
       (spit request-file request)
       (spit script
             (format #f "(use-modules (ice-9 textual-ports))~@
                          (use-modules (sift web))~@
                          (display (web-request (call-with-input-file ~s get-string-all)))~@
                          "
                     request-file))
       (let* ((line (string-join (append (list "guile")
                                         (compat-load-path directory)
                                         (list "-s" script))
                                 " "))
              (command (format #f "~a > ~a 2> ~a; echo $?"
                               line out-file err-file))
              (status (call-with-input-string
                        (let ((port (open-input-pipe command)))
                          (let ((answer (read-line port)))
                            (close-pipe port)
                            (or answer "1")))
                        read)))
         (assert-equal #:expect 0 #:got status
           #:name (if (zero? status)
                    "the stand-in loads and answers"
                    (format #f "the stand-in loads and answers; it said: ~a"
                            (string-trim-both (slurp err-file)))))
         (slurp out-file))))))

;;; ===== The suite =====

(define (spec)
  (suite "the browser facade"

    ;; --- what it is ---------------------------------------------------

    (test "the version is sift's version"
      (assert-equal #:expect sift-version
        #:got (field-of (result-of (asked (cons "op" "version"))) "version")))

    (test "a request that is not JSON is an error, not an exception"
      (let* ((answer (json-read (web-request "not json at all")))
             (error (json-ref answer "error")))
        (assert-equal #:expect json-false #:got (json-ref answer "ok")
          #:name "it is refused")
        (assert-equal #:expect "request" #:got (json-ref error "kind")
          #:name "as a request error")))

    (test "a request with no op is a usage error"
      (assert-refused #:request (asked) #:kind "usage"
                      #:name "no op"))

    (test "an unknown op is a usage error"
      (assert-refused #:request (asked (cons "op" "reticulate")) #:kind "usage"
                      #:name "unknown op"))

    (test "an unknown dialect is a usage error"
      (assert-refused
       #:request (asked (cons "op" "lint") (cons "source" "(a)")
                        (cons "dialect" "klingon"))
       #:kind "usage" #:name "unknown dialect"))

    (test "a source that is not a string is a usage error"
      (assert-refused
       #:request (asked (cons "op" "lint") (cons "source" 42))
       #:kind "usage" #:name "a number for a source"))

    ;; --- formatting ---------------------------------------------------

    (test "format answers the formatted text"
      (assert-equal
        #:expect "(define (square x)\n  (* x x))\n"
        #:got (field-of (result-of (asked (cons "op" "format")
                                          (cons "source" "(define (square x)\n(* x x))\n")))
                        "source")
        #:name "the indentation is the formatter's"))

    (test "format says whether it changed anything"
      (assert-equal
        #:expect json-false
        #:got (field-of (result-of (asked (cons "op" "format")
                                          (cons "source" "(a)\n")))
                        "changed")
        #:name "a formatted buffer is not changed")
      (assert-equal
        #:expect json-true
        #:got (field-of (result-of (asked (cons "op" "format")
                                          (cons "source" "(a )\n")))
                        "changed")
        #:name "an unformatted buffer is"))

    (test "the dialect reaches the formatter"
      (assert-equal
        #:expect "(cond ((= x 1) (a)))\n"
        #:got (field-of (result-of (asked (cons "op" "format")
                                          (cons "source" "(cond ((= x 1) (a)))\n")
                                          (cons "dialect" "elisp")))
                        "source")
        #:name "Emacs Lisp keeps its parentheses")
      (assert-equal
        #:expect "(cond [(= x 1) (a)])\n"
        #:got (field-of (result-of (asked (cons "op" "format")
                                          (cons "source" "(cond ((= x 1) (a)))\n")
                                          (cons "dialect" "scheme")))
                        "source")
        #:name "Scheme writes its clauses with brackets")
      (assert-equal
        #:expect "#lang racket\n(match value\n  [(list a b) (+ a b)])\n"
        #:got (field-of (result-of (asked (cons "op" "format")
                                          (cons "source" "#lang racket\n(match value\n((list a b) (+ a b)))\n")
                                          (cons "dialect" "racket")))
                        "source")
        #:name "Racket keeps its `#lang` line and brackets its match clauses"))

    (test "the style reaches the formatter"
      (assert-equal
        #:expect "(define (f)\n  (a)\n)\n"
        #:got (field-of (result-of (asked (cons "op" "format")
                                          (cons "source" "(define (f)\n  (a))\n")
                                          (cons "style" "dangling")))
                        "source")
        #:name "dangling closers are their own line"))

    (test "a format of a broken file answers the error rather than throwing"
      (let ((warnings (warnings-of (asked (cons "op" "format")
                                          (cons "source" "(a\n")))))
        (assert-equal #:expect 1 #:got (length warnings)
          #:name "the reader's finding is in the answer")
        (assert-equal #:expect "error"
          #:got (field-of (car warnings) "severity")
          #:name "at error severity")))

    ;; --- linting ------------------------------------------------------

    (test "lint answers one finding with its rule, code and place"
      (let* ((warnings (warnings-of
                        (asked (cons "op" "lint")
                               (cons "source" "(define (f)\n  (if a b))\n"))))
             (finding (car warnings)))
        (assert-equal #:expect 1 #:got (length warnings) #:name "one finding")
        (assert-equal #:expect "if-without-else" #:got (field-of finding "rule")
          #:name "named")
        (assert-equal #:expect "SIFT012" #:got (field-of finding "code")
          #:name "coded")
        (assert-equal #:expect "warning" #:got (field-of finding "severity")
          #:name "with a severity")
        (assert-equal #:expect 2 #:got (field-of finding "line") #:name "on line 2")
        (assert-equal #:expect 4 #:got (field-of finding "column")
          #:name "at column 4")
        (assert-true (number? (field-of finding "from"))
          #:name "with a range to underline")
        (assert-equal #:expect json-true #:got (field-of finding "fixable")
          #:name "and the news that it can be fixed")))

    (test "the counts add up"
      (let ((counts (field-of (result-of (asked (cons "op" "lint")
                                                (cons "source" "(a")))
                              "counts")))
        (assert-equal #:expect 1 #:got (field-of counts "error")
          #:name "one error for the unterminated form")
        (assert-equal #:expect 0 #:got (field-of counts "warning")
          #:name "no warnings")))

    (test "severity filters what is reported"
      (assert-equal
        #:expect 0
        #:got (length (warnings-of (asked (cons "op" "lint")
                                          (cons "source" "(define (f) x)\n")
                                          (cons "severity" "error"))))
        #:name "an info finding is below an error threshold"))

    (test "checkers selects the rules to run"
      (let* ((request (asked (cons "op" "lint")
                             (cons "source" "(define (f)\n  (if a b))\n")
                             (cons "checkers" (vector "if-without-else"))))
             (warnings (warnings-of request)))
        (assert-equal #:expect 1 #:got (length warnings)
          #:name "only the rule that was asked for")
        (assert-equal #:expect "if-without-else"
          #:got (field-of (car warnings) "rule") #:name "which is the one")))

    (test "the suppression comments a file carries are honoured"
      (let ((warnings (warnings-of
                       (asked (cons "op" "lint")
                              (cons "source"
                                    ";; sift:disable-next-line if-without-else\n(if a b)\n")))))
        (assert-equal
          #:expect #f
          #:got (find (lambda (warning)
                        (equal? (field-of warning "rule") "if-without-else"))
                      warnings)
          #:name "a disabled rule says nothing")
        (assert-true (> (length warnings) 0)
          #:name "and the rules around it still speak")))

    (test "a project macro can be given its indentation"
      (assert-equal
        #:expect "(with-thing\n  (a)\n  (b))\n"
        #:got (field-of (result-of
                         (asked (cons "op" "format")
                                (cons "source" "(with-thing\n(a)\n(b))\n")
                                (cons "indentRules" (vector (vector "with-thing" 2)))))
                        "source")
        #:name "the rule from the request reached the formatter"))

    ;; --- fixing -------------------------------------------------------

    (test "fix applies the fixes and says how many"
      (let ((result (result-of (asked (cons "op" "fix")
                                      (cons "source" "(define (f) (let ((x 1)) x))\n")))))
        (assert-equal #:expect "(define (f) 1)\n" #:got (field-of result "source")
          #:name "the let with one binding folded away")
        (assert-true (> (field-of result "applied") 0)
          #:name "and it says how many fixes it made")
        (assert-equal #:expect json-true #:got (field-of result "changed")
          #:name "and that the buffer changed")))

    (test "fix on a formatted buffer changes nothing"
      (let ((result (result-of (asked (cons "op" "fix")
                                      (cons "source" "(define (f) 1)\n")))))
        (assert-equal #:expect json-false #:got (field-of result "changed")
          #:name "nothing to do")))

    ;; --- the catalogue ------------------------------------------------

    (test "the rules are listed with their codes and dialects"
      (let* ((result (result-of (asked (cons "op" "checkers"))))
             (checkers (vector->list (field-of result "checkers")))
             (finding (find (lambda (checker)
                              (equal? (field-of checker "name") "if-without-else"))
                            checkers)))
        (assert-equal #:expect 93 #:got (length checkers) #:name "all of them")
        (assert-true (if finding #t #f)
          #:name "and the one we asked about is there")
        (assert-equal #:expect "SIFT012" #:got (field-of finding "code")
          #:name "with its code")
        (assert-equal #:expect "scheme"
          #:got (vector-ref (field-of finding "dialects") 0)
          #:name "its dialect")
        (assert-true (> (string-length (field-of finding "example")) 0)
          #:name "and an example to read")))

    (test "a rule that applies to every dialect lists them all"
      (let* ((checkers (vector->list (field-of (result-of (asked (cons "op" "checkers")))
                                               "checkers")))
             (tabs (find (lambda (checker) (equal? (field-of checker "name") "tabs"))
                         checkers)))
        (assert-equal #:expect 4 #:got (vector-length (field-of tabs "dialects"))
          #:name "symbol `all' is spelled out")))

    ;; --- the answers are portable -------------------------------------

    (test "the answer is JSON, and reads back"
      (let ((text (web-request "{\"op\":\"version\"}")))
        (assert-true (string? text) #:name "a string comes back")
        (assert-true (json-object? (json-read text))
          #:name "and it is a document sift can read again")))

    ;; --- the questions an editor asks --------------------------------

    (test "an expression is extracted to a let, and the buffer comes back"
      (let* ((source "(define (area w h)\n  (+ (* w h) 1))\n")
             (result (result-of (asked (cons "op" "extract")
                                       (cons "source" source)
                                       (cons "offset" 26)
                                       (cons "to" "let"))))
             (rewritten (field-of result "source")))
        (assert-equal #:expect json-true #:got (json-ref result "changed")
          #:name "the answer says the buffer moved")
        (assert-contains #:expect rewritten #:got "(let ((value (* w h))) (+ value 1))"
          #:name "the expression is bound where it was written")
        (assert-equal #:expect "(define (area w h)\n  (let ((value (* w h))) (+ value 1)))\n"
          #:got rewritten
          #:name "and the definition around it is intact")))

    (test "extracting where there is nothing to extract is a usage failure"
      (assert-refused #:kind "usage"
                      #:name "a definition is not an expression"
                      #:request (asked (cons "op" "extract")
                                       (cons "source" "(define (f) 1)\n")
                                       (cons "offset" 2))))

    (test "the outline names what the buffer defines"
      (let* ((result (result-of (asked (cons "op" "symbols")
                                       (cons "source" "(define (square x)\n  (* x x))\n"))))
             (symbols (vector->list (field-of result "symbols"))))
        (assert-equal #:expect 1 #:got (length symbols)
          #:name "one top-level form")
        (assert-equal #:expect "square" #:got (field-of (car symbols) "name")
          #:name "named after the definition")
        (assert-true (json-object? (field-of (car symbols) "range"))
          #:name "with a range to draw it at")))

    (test "a definition in the buffer is found"
      (let* ((result (result-of (asked (cons "op" "definition")
                                       (cons "source" "(define (square x) x)\n(square 3)\n")
                                       (cons "offset" 24))))
             (definitions (vector->list (field-of result "definitions"))))
        (assert-equal #:expect 1 #:got (length definitions)
          #:name "the one definition there is")
        (assert-equal #:expect 0
          #:got (field-of (field-of (field-of (car definitions) "range") "start") "line")
          #:name "on the line it is written on")))

    (test "a hover describes the symbol under the cursor"
      (let* ((result (result-of (asked (cons "op" "hover")
                                       (cons "source" "(define (square x) x)\n(square 3)\n")
                                       (cons "offset" 24))))
             (hover (field-of result "hover")))
        (assert-true (json-object? hover) #:name "there is an answer")
        (assert-true (json-object? (field-of hover "contents"))
          #:name "with contents the editor can draw")))

    (test "a hover where there is nothing to say answers null"
      (let ((result (result-of (asked (cons "op" "hover")
                                      (cons "source" "(define (square x) x)\n(\n")
                                      (cons "offset" 22)))))
        (assert-equal #:expect json-null #:got (field-of result "hover")
          #:name "no hover, and no error either")))

    (test "completion offers the names the buffer defines"
      (let* ((result (result-of (asked (cons "op" "completion")
                                       (cons "source" "(define (square x) x)\n(sq")
                                       (cons "offset" 25))))
             (items (vector->list (field-of result "items"))))
        (assert-equal #:expect "square" #:got (field-of (car items) "label")
          #:name "the name that starts with what is typed")))

    (test "the highlights tell a declaration from a use"
      (let* ((result (result-of (asked (cons "op" "highlight")
                                       (cons "source" "(define (square x) x)\n(square 3)\n")
                                       (cons "offset" 9))))
             (highlights (vector->list (field-of result "highlights"))))
        (assert-equal #:expect 2 #:got (length highlights)
          #:name "the declaration and the call")
        (assert-true (not (equal? (field-of (car highlights) "kind")
                                  (field-of (cadr highlights) "kind")))
          #:name "marked apart")))

    (test "inlay hints name the arguments a call passes"
      (let* ((result (result-of (asked (cons "op" "inlayHints")
                                       (cons "source"
                                             "(define (area width height) 0)\n(area 2 3)\n"))))
             (hints (vector->list (field-of result "hints"))))
        (assert-equal #:expect 2 #:got (length hints)
          #:name "one per positional argument")
        (assert-equal #:expect " width " #:got (field-of (car hints) "label")
          #:name "named by the formal list, with one space on each side")))

    (test "a question without an offset is a usage error"
      (assert-refused #:request (asked (cons "op" "hover") (cons "source" "(a)"))
                      #:kind "usage" #:name "hover with no offset"))

    ;; --- the seam between the browser and the core --------------------

    (test "the core does not know the browser exists"
      (let ((offenders
             (filter-map
              (lambda (path)
                (let* ((text (slurp path))
                       (word (browser-word-in text)))
                  (and word (cons (relative-to-root path) word))))
              (core-sources))))
        (assert-equal #:expect '() #:got offenders
          #:name "no file under src/sift/ names the browser, Wasm or Hoot")))

    (test "the facade imports five core modules, and no more"
      (assert-equal
        #:expect '((sift analysis) (sift format) (sift json) (sift lint) (sift records))
        #:got (sort (core-modules-used-by-facade) module-name<?)
        #:name "the browser build's dependency on the core is this list"))

    (test "every Guile library the browser build imports has a stand-in"
      (let ((unprovided (remove (lambda (name)
                                  (or (shimmed? name)
                                    (member name hoot-shipped-libraries)))
                                (ice-9-libraries-in-closure))))
        (assert-equal #:expect '() #:got unprovided
          #:name "web/compat covers what the closure imports and the target lacks")))

    ;; --- the page, and the directory it is served from -----------------

    ;; https://jointhefreeworld.org/sift/ is a directory beside a blog, not a
    ;; site of its own, and it is served with a long cache.  Two mistakes in a
    ;; page like that are invisible until a visitor makes them: a URL from `/'
    ;; is a 404 the moment the editor is not at the root, and a URL that does
    ;; not change when the build does is yesterday's editor, cached.

    (test "the page names no path from the root of the site"
      (let ((absolute (filter (lambda (url)
                                (or (string-prefix? "/" url)
                                  (string-contains url "://")))
                              (page-urls))))
        (assert-equal #:expect '() #:got absolute
          #:name "every URL is relative, so the editor works in a subdirectory")))

    (test "every URL the page names carries the build stamp"
      (assert-true (> (length (page-urls)) 0)
        #:name "the page names something")
      (let ((unstamped (remove (lambda (url)
                                 (string-contains url "__SIFT_BUILD__"))
                               (page-urls))))
        (assert-equal #:expect '() #:got unstamped
          #:name "a URL that does not change is a deploy a cache hides")))

    (test "the editor asks for the wasm beside itself"
      (let ((text (slurp (web-file "editor.js"))))
        (assert-true (if (string-contains text "import.meta.url") #t #f)
          #:name "the module's own URL is where sift.wasm is found")
        (assert-equal #:expect #f #:got (string-contains text "\"./sift.wasm\"")
          #:name "and not a path from wherever the page happens to be served")))

    ;; --- the compatibility layer --------------------------------------

    (test "the browser's SRFI-13 is the stand-in web/compat carries"
      ;; Hoot has srfi-9, srfi-11 and srfi-14 and no SRFI-13, and Guile's own
      ;; srfi-13 is written with a `re-export' Hoot does not implement, so the
      ;; core's import of it is answered here or the build fails by name.
      (assert-true (shimmed? '(srfi srfi-13))
        #:name "web/compat/srfi/srfi-13.scm is what a browser build finds"))

    (test "the browser's SRFI-13 answers what Guile's does"
      (for-each
       (lambda (question)
         (let ((request (json-write question)))
           (assert-equal #:expect (web-request request)
             #:got (answer-over-compat request)
             #:name (format #f "the ~a answer is the same over the stand-in"
                            (field-of question "op")))))
       (list
        ;; a directive comment, which is trimmed and indexed, over a buffer that
        ;; is not formatted yet
        (asked (cons "op" "format")
               (cons "source"
                     ";; sift:disable-next-line if-without-else\n(define (square x)\n(* x x))\n"))
        ;; a rule that fires, so the answer carries the lint path
        (asked (cons "op" "lint")
               (cons "source" "(define (f)\n  (if a b))\n"))
        ;; and a fix run, which reads the buffers it writes
        (asked (cons "op" "fix")
               (cons "source" "(define (f) (let ((x 1)) x))\n")))))

    (test "the stand-in's two late additions answer what Guile's do"
      ;; `string-upcase' and `string-drop-right' are the two names the core uses
      ;; that neither R7RS nor Hoot's `(guile)' defines -- the first writes the
      ;; hex of a JSON escape, the second strips `.gz' before a dialect is
      ;; decided.  Neither is reachable through a request, so they are compared
      ;; where they are: in the stand-in's own process.
      (let ((questions
             `(("(string-upcase \"1b\")" . ,(string-upcase "1b"))
               ("(string-upcase \"a-z_0\")" . ,(string-upcase "a-z_0"))
               ("(string-drop-right \"notes.el.gz\" 3)"
                . ,(string-drop-right "notes.el.gz" 3))
               ("(string-drop-right \"no-extension\" 0)"
                . ,(string-drop-right "no-extension" 0)))))
        (assert-equal #:expect (map (lambda (question)
                                      (format #f "~s" (cdr question)))
                                    questions)
          #:got (stand-in-answers (map car questions))
          #:name "the stand-in and the library agree, answer for answer")))))
