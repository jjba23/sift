;; sift --- test helpers
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Veritas is the only test framework sift uses, and this module is where the
;; project-specific assertions live: reading a fixture, comparing two
;; strings with a message that says where they first differ, running one
;; checker over one snippet, and applying that checker's fixes.
;;
;; Everything here returns a Veritas assertion, never a bare boolean.  If an
;; assertion is missing from Veritas, the helper that would provide it belongs
;; here -- not a second assertion library.

(define-module (tests helpers)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift reader)
  #:use-module (sift format)
  #:use-module (sift lint)
  #:use-module (sift config)
  #:export (test-root
            fixture
            slurp
            spit
            first-difference
            assert-same-text
            assert-warnings
            assert-fix
            assert-fixes-to-nothing
            assert-warning-count
            assert-idempotent
            warnings-of
            findings-of
            node->sexp
            ast->sexp
            read-sexps
            read-errors
            keep-temp-directory
            make-directories
            remove-tree
            with-temp-directory
            in-temp-directory
            run-sift
            checker-names
            fixture-pairs
            style-of
            dialect-of
            assert-fixture
            assert-fixture-idempotent))

;;; ===== Files =====

(define (test-root)
  "Return the root of the checkout the suite runs in.

`maak test' passes -L ./tests from the repository root, so the working
directory is the root; SIFT_TEST_ROOT overrides it for a runner that starts
somewhere else."
  (or (getenv "SIFT_TEST_ROOT") (getcwd)))

(define (fixture . parts)
  "Return the absolute path of a file under tests/fixtures."
  (string-join (cons (string-append (test-root) "/tests/fixtures") parts) "/"))

(define (slurp path)
  (call-with-input-file path get-string-all))

(define (spit path contents)
  (let ((directory (dirname path)))
    (unless (file-exists? directory) (make-directories directory)))
  (call-with-output-file path
                         (lambda (port) (display contents port))))

;;; ===== Comparing text =====

(define (first-difference expected actual)
  "Return a sentence naming where two strings first differ.

A failing golden test that only says \"not equal\" leaves the reader to find
the difference by eye; a byte offset and the two lines around it do not."
  (let* ((limit (min (string-length expected) (string-length actual))))
    (let loop ((i 0))
      (cond
        [(>= i limit)
         (format #f "the texts agree for ~a characters and one of them continues: expected ~a characters, got ~a"
                 limit (string-length expected) (string-length actual))]
        [(not (char=? (string-ref expected i) (string-ref actual i)))
         (format #f "characters differ at offset ~a (runes ~s and ~s): expected ~s, got ~s"
                 i
                 (char->integer (string-ref expected i))
                 (char->integer (string-ref actual i))
                 (context-around expected i)
                 (context-around actual i))]
        [else (loop (+ i 1))]))))

(define (context-around text index)
  (let* ((from (max 0 (- index 20)))
         (to (min (string-length text) (+ index 20))))
    (substring text from to)))

(define* (assert-same-text #:key expect got
                           (name "the text matches"))
  "Assert that two strings are equal, with a usable message when they are not."
  (assert-equal #:expect expect #:got got
    #:name (if (equal? expect got)
             name
             (format #f "~a [~a]" name
                     (first-difference expect got)))))

(define* (assert-idempotent #:key source (dialect 'scheme)
                            (style 'community)
                            (name "formatting is a fixed point"))
  "Assert that formatting @var{source} twice changes nothing the second time."
  (let* ((once (format-string source #:dialect dialect #:style style))
         (twice (format-string once #:dialect dialect #:style style)))
    (assert-same-text #:expect once #:got twice #:name name)))

;;; ===== Running one checker =====

(define* (warnings-of source #:key (dialect 'scheme) (checker #f)
                      (options '()))
  "Return the warnings @var{checker} reports for @var{source}."
  (lint-source source #:dialect dialect
               #:checkers (and checker (list checker))
               #:options options))

(define (findings-of warnings)
  (map (lambda (warning)
         (list (source-location-line (lint-warning-location warning))
               (source-location-column (lint-warning-location warning))))
       warnings))

(define* (assert-warnings #:key source checker
                          (dialect 'scheme)
                          expect
                          (options '())
                          (name "the findings are where they should be"))
  "Assert that @var{checker} reports findings at exactly these positions.

Each expected position is @code{(line column)}; an empty list means the
checker is silent, which is the other half of every checker test."
  (let* ((warnings (warnings-of source #:dialect dialect #:checker checker
                                #:options options))
         (found (findings-of warnings)))
    (assert-equal #:expect expect #:got found
      #:name (if (equal? expect found)
               name
               (format #f "~a [reported ~s: ~a]" name found
                       (map lint-warning->string warnings))))))

(define* (assert-warning-count #:key source checker (dialect 'scheme)
                               (count 1)
                               (name "the number of findings is right"))
  "Assert how many findings @var{checker} reports for @var{source}.

Some checkers -- `provide-missing' is one -- report a fact about the file
rather than about a position in it, and the position they choose is not
worth pinning down."
  (let ((found (warnings-of source #:dialect dialect #:checker checker)))
    (assert-equal #:expect count #:got (length found)
      #:name (if (= count (length found))
               name
               (format #f "~a [reported ~a]" name
                       (map lint-warning->string found))))))

(define* (assert-fix #:key source checker
                     (dialect 'scheme)
                     expected
                     (options '())
                     (name "the fix produces the expected text"))
  "Assert that applying @var{checker}'s fixes to @var{source} gives @var{expected}."
  (let* ((warnings (warnings-of source #:dialect dialect #:checker checker
                                #:options options))
         (fixed (apply-fixes source warnings)))
    (assert-same-text #:expect expected #:got fixed #:name name)))

(define* (assert-fixes-to-nothing #:key source checker
                                  (dialect 'scheme)
                                  (name "the fixed text is clean"))
  "Assert that re-linting the fixed text reports nothing.

A fix that leaves the finding in place is worse than no fix: the next run
reports it again and the user learns to ignore the checker."
  (let* ((warnings (warnings-of source #:dialect dialect #:checker checker))
         (fixed (apply-fixes source warnings))
         (remaining (findings-of (warnings-of fixed #:dialect dialect
                                              #:checker checker))))
    (assert-equal #:expect '() #:got remaining #:name name)))

;;; ===== Reading =====

(define* (node->sexp node)
  "Return a node as ordinary Scheme data, for structural assertions.

Symbols become symbols, numbers numbers, strings strings; lists become
lists, vectors vectors, and a reader prefix becomes the long-hand form it
abbreviates, so a test can say what the reader understood without knowing
how a node is built."
  (cond
    [(node-list? node)
     (let ((elements (map node->sexp (node-elements node))))
       (if (node-vector? node) (list->vector elements) elements))]
    [(node-prefix? node)
     ;; The token is compared as text: a Scheme reader is free to intern the
     ;; symbol `quote' and the symbol named by a lone quote mark differently,
     ;; and a test should not care which it did.
     (let ((child (and (node-child node) (node->sexp (node-child node))))
           (token (node-token node)))
       (cond
         [(string=? token "'") (list 'quote child)]
         [(string=? token "`") (list 'quasiquote child)]
         [(string=? token ",") (list 'unquote child)]
         [(string=? token ",@") (list 'unquote-splicing child)]
         [(string=? token "#'") (list 'function child)]
         [(string=? token "#;") (list 'datum-comment child)]
         [else (list (string->symbol token) child)]))]
    [(node-stray-delimiter? node) (node-value node)]
    [(node-atom? node)
     (case [node-atom-type node]
       [(number) (or (node-field node 'value #f) (node-value node))]
       [(boolean) (string=? (node-value node) "#t")]
       [(character string keyword) (node-value node)]
       [else (string->symbol (node-value node))])]
    [else (node-value node)]))

(define* (ast->sexp source #:key (dialect 'scheme))
  "Read @var{source} and return its data nodes as ordinary Scheme data."
  (call-with-values (lambda () (read-source #:source source #:dialect dialect))
    (lambda (ast errors) (map node->sexp (filter node-data? ast)))))

(define* (read-sexps source #:key (dialect 'scheme))
  (ast->sexp source #:dialect dialect))

(define* (read-errors source #:key (dialect 'scheme))
  "Return the parse errors of @var{source} as @code{(line column segment)}.

The message is cut down to its first word, which is the part a test cares
about: whether the reader said \"unterminated\", \"unmatched\" or nothing."
  (call-with-values (lambda () (read-source #:source source #:dialect dialect))
    (lambda (ast errors)
      (map (lambda (warning)
             (let ((location (lint-warning-location warning)))
               (list (source-location-line location)
                     (source-location-column location)
                     (string->symbol
                      (car (string-split (lint-warning-message warning) #\space))))))
           errors))))

(define (checker-names)
  (map lint-checker-name (all-checkers)))

;;; ===== Temporary directories =====

(define (make-directories path)
  "Create @var{path} and every missing parent, like `mkdir -p'."
  (unless (or (file-exists? path) (string=? path "/") (string-null? path))
    (make-directories (dirname path))
    (catch 'system-error
           (lambda () (mkdir path))
           (lambda (key . args)
             (unless (file-exists? path) (apply throw key args))))))

(define (remove-tree path)
  "Remove @var{path}, recursing into it when it is a directory.

The question is asked with `lstat', because a dangling symlink neither exists
nor is a directory: `file-exists?' says no, so a tree holding one could not be
taken apart, and the failure read as a test that would not clean up.

A path that goes away between the question and the answer is a path that is
gone.  A cleanup that raises on that turns one test's leftovers into another
test's failure, and the report then names a test that was working."
  (let ((info (catch #t (lambda () (lstat path)) (lambda (key . args) #f))))
    (when info
      (catch #t
             (lambda ()
               (if (eq? (stat:type info) 'directory)
                 (begin
                   (for-each (lambda (name)
                               (unless (member name '("." ".."))
                                 (remove-tree (string-append path "/" name))))
                             (or (scandir path) '()))
                   (rmdir path))
                 (delete-file path)))
             (lambda (key . args) #f)))))

(define temp-name-counter 0)

(define (unique-temp-path suffix)
  "Return a path under the temporary directory that no other test is using.

A name made of the process id alone collides as soon as two tests run
concurrently, which Veritas does by default; the counter is what makes a
run with several suites in flight behave like a run with one."
  (set! temp-name-counter (+ temp-name-counter 1))
  (format #f "~a/sift-test-~a-~a~a"
          (or (getenv "TMPDIR") "/tmp")
          (getpid) temp-name-counter suffix))

(define (keep-temp-directory)
  (or (getenv "SIFT_TEST_KEEP")
    (unique-temp-path "")))

(define (with-temp-directory thunk)
  "Call @var{thunk} with a fresh directory, removing it afterwards.

Every test that touches the file system -- `--write', `--fix', a real
`check' -- runs in one of these, so no test can see another's leftovers."
  (let ((directory (keep-temp-directory)))
    (when (file-exists? directory)
      (remove-tree directory))
    (make-directories directory)
    (dynamic-wind
      (lambda () #f)
      (lambda () (thunk directory))
      (lambda ()
        (unless (getenv "SIFT_TEST_KEEP")
          (remove-tree directory))))))

(define (in-temp-directory directory thunk)
  (let ((previous (getcwd)))
    (chdir directory)
    (dynamic-wind
      (lambda () #f)
      (lambda () (thunk))
      (lambda () (chdir previous)))))

;;; ===== The golden corpus =====

(define (fixture-pairs)
  "Return every @code{(input . expected)} pair in the corpus, sorted by name.

The corpus is the specification: a file is added once, and every pass over it
-- formatting, idempotence, already-formatted -- picks it up without another
list to keep in step."
  (let* ((root (string-append (test-root) "/tests/fixtures"))
         (dialects (sort (scandir root (lambda (name)
                                         (not (member name '("." "..")))))
                         string<?)))
    (append-map
     (lambda (dialect)
       (let ((styles (sort (scandir (string-append root "/" dialect)
                                    (lambda (name)
                                      (not (member name '("." "..")))))
                           string<?)))
         (append-map
          (lambda (style)
            (let ((input-dir (string-append root "/" dialect "/" style "/input"))
                  (expected-dir (string-append root "/" dialect "/" style "/expected")))
              (if (file-exists? input-dir)
                (map (lambda (name)
                       (cons (string-append input-dir "/" name)
                             (string-append expected-dir "/" name)))
                     (sort (scandir input-dir
                                    (lambda (name)
                                      (not (member name '("." "..")))))
                           string<?))
                '())))
          styles)))
     dialects)))

(define (style-of path)
  (let ((parts (string-split path #\/)))
    (if (member "dangling" parts) 'dangling 'community)))

(define (dialect-of path)
  (let ((parts (string-split path #\/)))
    (cond [(member "racket" parts) 'racket]
          [(member "elisp" parts) 'elisp]
          [(member "cl" parts) 'cl]
          [else 'scheme])))

(define* (assert-fixture pair)
  "Assert that the input of @var{pair} formats to the expected file."
  (let* ((input (car pair))
         (expected (cdr pair))
         (name (basename input)))
    (assert-same-text
     #:expect (slurp expected)
     #:got (format-string (slurp input)
                          #:dialect (dialect-of input)
                          #:style (style-of input))
     #:name (format #f "~a formats to its expected file" name))))

(define* (assert-fixture-idempotent pair)
  "Assert that formatting the expected file of @var{pair} changes nothing."
  (let* ((expected (cdr pair))
         (name (basename expected)))
    (assert-same-text
     #:expect (slurp expected)
     #:got (format-string (slurp expected)
                          #:dialect (dialect-of expected)
                          #:style (style-of expected))
     #:name (format #f "~a is already formatted" name))))

;;; ===== Running the executable =====

(define* (run-sift arguments #:key (directory (test-root)) (stdin #f))
  "Run bin/sift as a subprocess and return @code{(status stdout stderr)}.

The three come back as one list rather than as three values, because a test
that wants to report all of them in one message wants them in one place.  A
test that asserts an exit status has to see the status the shell would see,
which means a real process rather than a call to `sift-main'."
  (let* ((command (string-join
                   (append (list (string-append (test-root) "/bin/sift"))
                           arguments)
                   " "))
         (stdin-file (and stdin
                       (let ((path (unique-temp-path ".stdin")))
                         (spit path stdin)
                         path)))
         (out-file (unique-temp-path ".out"))
         (err-file (string-append out-file ".err"))
         (full (format #f "cd ~a && ~a < ~a > ~a 2> ~a; echo $?"
                       directory command
                       (or stdin-file "/dev/null")
                       out-file err-file))
         (status (call-with-input-string
                   (let ((port (open-input-pipe full)))
                     (let ((line (read-line port)))
                       (close-pipe port)
                       (or line "1")))
                   read)))
    (let ((out (if (file-exists? out-file) (slurp out-file) ""))
          (err (if (file-exists? err-file) (slurp err-file) "")))
      (for-each (lambda (path) (when (file-exists? path) (delete-file path)))
                (filter identity (list stdin-file out-file err-file)))
      (list status out err))))
