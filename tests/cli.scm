;; sift --- command line tests
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; The command line is run as a subprocess, not as a procedure call, because
;; what is being tested is the contract a shell sees: the exit status, what
;; went to stdout, and what went to stderr.  A run that touches the file
;; system does so inside a fresh temporary directory.

(define-module (tests cli)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (tests helpers)
  #:export (spec))

(define* (assert-status #:key expect got
           (name "the exit status is right"))
  (assert-equal #:expect expect #:got (car got)
    #:name (if (= expect (car got))
             name
             (format #f "~a [stderr: ~a]" name (caddr got)))))

(define (occurrences needle haystack)
  "Return how many times @var{needle} appears in @var{haystack}.

A format that is one document per run is asserted by counting what a document
has exactly one of, which is a question `assert-contains' cannot ask."
  (let loop ((start 0) (count 0))
    (let ((at (string-contains haystack needle start)))
      (if at
        (loop (+ at (string-length needle)) (+ count 1))
        count))))

(define (spec)
  ;; serial: every test here starts a process, and a dozen of them at once
  ;; turns a timing accident into a flaky suite
  (list
     (suite "cli"
       #:concurrent? #f

       (test "version prints the version and succeeds"
         (let ((result (run-sift '("version"))))
           (assert-status #:expect 0 #:got result)
           (assert-contains #:expect (cadr result) #:got "sift ")))

       (test "format writes the formatted source to stdout"
         (let* ((result (run-sift '("format" "tests/fixtures/scheme/community/input/messy.scm")))
                (expected (slurp (fixture "scheme" "community" "expected" "messy.scm"))))
           (assert-status #:expect 0 #:got result)
           (assert-same-text #:expect expected #:got (cadr result))))

       (test "check reports a difference with status 1"
         (let ((result (run-sift '("check" "tests/fixtures/scheme/community/input/messy.scm"))))
           (assert-status #:expect 1 #:got result)
           (assert-contains #:expect (cadr result) #:got "@@ ")))

       (test "check on an already formatted file succeeds"
         (let ((result (run-sift '("check" "tests/fixtures/scheme/community/expected/messy.scm"))))
           (assert-status #:expect 0 #:got result)
           (assert-same-text #:expect "" #:got (cadr result))))

       (test "format --write rewrites the file in place"
         (with-temp-directory
            (lambda (directory)
              (let ((path (string-append directory "/messy.scm")))
                (spit path "(define (f x)\n(+ x 1))\n")
                (let ((result (run-sift (list "format" "--write" path))))
                  (assert-status #:expect 0 #:got result)
                  (assert-same-text
                     #:expect "(define (f x)\n  (+ x 1))\n"
                     #:got (slurp path)))))))

       (test "lint reports findings with status 1"
         (let ((result (run-sift '("lint" "tests/fixtures/lint/dirty.scm"))))
           (assert-status #:expect 1 #:got result)
           (assert-contains #:expect (cadr result) #:got "[trailing-whitespace]")))

       (test "lint on a clean file succeeds and prints a summary"
         (let ((result (run-sift '("lint" "tests/fixtures/lint/clean.scm"))))
           (assert-status #:expect 0 #:got result)
           (assert-contains #:expect (cadr result) #:got "1 file checked")))

       (test "lint --quiet keeps the findings and drops the summary"
         (let ((result (run-sift '("lint" "--quiet" "tests/fixtures/lint/dirty.scm"))))
           (assert-status #:expect 1 #:got result)
           (assert-contains #:expect (cadr result) #:got "[trailing-whitespace]")
           (assert-not-contains #:expect (cadr result) #:got "file checked")))

       (test "lint --format=json emits one array for the run"
         (let* ((result (run-sift '("lint" "--format=json" "tests/fixtures/lint/dirty.scm")))
                (output (cadr result)))
           (assert-status #:expect 1 #:got result)
           (assert-true (string-prefix? "[" output) #:name "it starts with an array")
           (assert-contains #:expect output #:got "\"severity\"")
           (assert-contains #:expect output #:got "\"checker\"")
           (assert-contains #:expect output #:got "\"line\"")))

       (test "lint --format=null says nothing at all"
         (let ((result (run-sift '("lint" "--format=null" "tests/fixtures/lint/dirty.scm"))))
           (assert-status #:expect 1 #:got result)
           (assert-same-text #:expect "" #:got (cadr result))))

       (test "lint --format=json carries the rule code"
         (let* ((result (run-sift '("lint" "--format=json" "tests/fixtures/lint/dirty.scm")))
                (output (cadr result)))
           (assert-contains #:expect output #:got "\"code\"")
           (assert-contains #:expect output #:got "SIFT001")))

       (test "list-checkers prints each rule's code"
         (let ((result (run-sift '("list-checkers"))))
           (assert-contains #:expect (cadr result) #:got "SIFT001")
           (assert-contains #:expect (cadr result) #:got "CODE")))

       (test "explain leads with the rule's code"
         (let ((result (run-sift '("explain" "if-without-else"))))
           (assert-contains #:expect (cadr result) #:got "SIFT012")))

       (test "lint --checker runs only the checker it names"
         (let* ((result (run-sift '("lint" "--format=json" "--checker=line-length"
                                    "tests/fixtures/lint/dirty.scm")))
                (output (cadr result)))
           (assert-status #:expect 0 #:got result)
           (assert-not-contains #:expect output #:got "trailing-whitespace")))

       (test "lint --exclude-checker skips the checker it names"
         (let* ((result (run-sift '("lint" "--format=json" "--exclude-checker=trailing-whitespace"
                                    "tests/fixtures/lint/dirty.scm")))
                (output (cadr result)))
           (assert-not-contains #:expect output #:got "trailing-whitespace")))

       (test "lint --fix rewrites the file and reports what is left"
         (with-temp-directory
            (lambda (directory)
              (let ((path (string-append directory "/dirty.scm")))
                (spit path "(if (ready?) (go))   \n")
                (let ((result (run-sift (list "lint" "--fix" path))))
                  (assert-status #:expect 0 #:got result)
                  (assert-same-text #:expect "(when (ready?) (go))\n"
                    #:got (slurp path)))))))

       (test "format --diff prints a patch and exits 1"
         (let ((result (run-sift '("format" "--diff"
                                   "tests/fixtures/scheme/community/input/messy.scm"))))
           (assert-status #:expect 1 #:got result)
           (assert-contains #:expect (cadr result) #:got "@@ ")
           (assert-contains #:expect (cadr result) #:got "-")))

       (test "format --diff on a formatted file prints nothing and exits 0"
         (let ((result (run-sift '("format" "--diff"
                                   "tests/fixtures/scheme/community/expected/messy.scm"))))
           (assert-status #:expect 0 #:got result)
           (assert-same-text #:expect "" #:got (cadr result))))

       (test "--version prints the version without a subcommand"
         (let ((result (run-sift '("--version"))))
           (assert-status #:expect 0 #:got result)
           (assert-contains #:expect (cadr result) #:got "sift ")))

       (test "fix is lint --fix"
         (with-temp-directory
            (lambda (directory)
              (let ((path (string-append directory "/fixme.scm")))
                (spit path "(if (ready?) (go))   \n")
                (let ((result (run-sift (list "fix" path))))
                  (assert-status #:expect 0 #:got result)
                  (assert-same-text #:expect "(when (ready?) (go))\n" #:got (slurp path)))))))

       (test "lint --fix --stdin prints the fixed source to stdout"
         (let ((result (run-sift '("lint" "--fix" "--stdin" "--dialect=scheme")
                         #:stdin "(if (ready?) (go))   \n")))
           (assert-status #:expect 0 #:got result)
           (assert-same-text #:expect "(when (ready?) (go))\n" #:got (cadr result))))

       (test "--label names the source in diagnostics"
         (let ((result (run-sift '("lint" "--stdin" "--dialect=scheme"
                                   "--label=my-buffer.scm" "--format=human")
                         #:stdin "(if (ready?) (go))\n")))
           (assert-status #:expect 1 #:got result)
           (assert-contains #:expect (cadr result) #:got "my-buffer.scm")))

       (test "--brackets=parens overrides the Scheme default"
         (let ((result (run-sift '("format" "--stdin" "--dialect=scheme"
                                   "--brackets=parens")
                         #:stdin "(cond ((< n 0) 1)\n      (else 2))\n")))
           (assert-status #:expect 0 #:got result)
           (assert-contains #:expect (cadr result) #:got "(cond ((< n 0) 1)")))

       ;; `--name value' and `--name=value' have to mean the same thing for every
       ;; option that takes a value, and the list of those options is read off the
       ;; specification rather than kept beside it.  This is the test that would
       ;; have caught `--brackets square' and `--label x' being rejected: the two
       ;; spellings were written by hand in a list that drifted.
       (test "every option that takes a value accepts both spellings"
         (let* ((pairs '(("--dialect" . "scheme")
                         ("--severity" . "warning")
                         ("--format" . "null")
                         ("--style" . "community")
                         ("--line-length" . "90")
                         ("--indent" . "4")
                         ("--brackets" . "square")
                         ("--label" . "spelled.scm")))
                (source "(define (f x)\n(+ x 1))\n")
                (run (lambda (args)
                       (run-sift (append '("format" "--stdin" "--dialect=scheme") args)
                         #:stdin source)))
                (given (map (lambda (pair)
                              (list (car pair)
                                (run (list (car pair) (cdr pair)))
                                (run (list (string-append (car pair) "=" (cdr pair))))))
                         pairs)))
           (for-each
              (lambda (entry)
                (let ((space (cadr entry))
                      (equals (caddr entry)))
                  (assert-status #:expect 0 #:got space
                    #:name (format #f "~a value is accepted" (car entry)))
                  (assert-same-text #:expect (cadr equals) #:got (cadr space)
                    #:name (format #f "~a spells the same both ways" (car entry)))))
              given)))


       (test "an unknown bracket style is a usage error"
         (let ((result (run-sift '("format" "--brackets=curly"
                                   "tests/fixtures/lint/clean.scm"))))
           (assert-status #:expect 2 #:got result)
           (assert-contains #:expect (caddr result) #:got "unknown bracket style")))

       (test "--stdin and --stdout work as a filter"
         (let ((result (run-sift '("format" "--stdin" "--stdout" "--dialect=scheme")
                         #:stdin "(define (f x)\n(+ x 1))\n")))
           (assert-status #:expect 0 #:got result)
           (assert-same-text #:expect "(define (f x)\n  (+ x 1))\n"
             #:got (cadr result))))

       (test "a parse error is a refusal, with status 2"
         (with-temp-directory
            (lambda (directory)
              (let ((path (string-append directory "/broken.scm")))
                (spit path "(define (f x)\n  (a b\n")
                (let ((result (run-sift (list "format" path))))
                  (assert-status #:expect 2 #:got result)
                  (assert-contains #:expect (caddr result) #:got "parse error"))))))

       (test "--force formats what can be read"
         (with-temp-directory
            (lambda (directory)
              (let ((path (string-append directory "/broken.scm")))
                (spit path "(define (f x)\n  (a b\n")
                (let ((result (run-sift (list "format" "--force" path))))
                  (assert-status #:expect 0 #:got result))))))

       (test "an unknown command is a usage error, with status 2"
         (let ((result (run-sift '("frobnicate"))))
           (assert-status #:expect 2 #:got result)
           (assert-contains #:expect (caddr result) #:got "Usage:")))

       (test "an unknown dialect is a usage error"
         (let ((result (run-sift '("lint" "--dialect=pascal" "tests/fixtures/lint/clean.scm"))))
           (assert-status #:expect 2 #:got result)
           (assert-contains #:expect (caddr result) #:got "unknown dialect")))

       (test "--write with --stdout is a usage error"
         (let ((result (run-sift '("format" "--write" "--stdout"
                                   "tests/fixtures/lint/clean.scm"))))
           (assert-status #:expect 2 #:got result)
           (assert-contains #:expect (caddr result) #:got "cannot be used together")))

       (test "an unknown checker is a usage error"
         (let ((result (run-sift '("lint" "--checker=nope" "tests/fixtures/lint/clean.scm"))))
           (assert-status #:expect 2 #:got result)
           (assert-contains #:expect (caddr result) #:got "unknown checker")))

       (test "an unreadable file is a usage error"
         (let ((result (run-sift '("format" "tests/fixtures/no-such-file.scm"))))
           (assert-status #:expect 2 #:got result)
           (assert-contains #:expect (caddr result) #:got "cannot read")))

       ;; Guile hands a `system-error' over with a format string where a
       ;; message would be, so printing it used to tell the user `~A: ~S'
       ;; instead of what went wrong with which file.
       (test "an unreadable file says why, in words"
         (let ((result (run-sift '("format" "tests/fixtures/no-such-file.scm"))))
           (assert-status #:expect 2 #:got result)
           (assert-contains #:expect (caddr result)
             #:got "no-such-file.scm")
           (assert-not-contains #:expect (caddr result) #:got "~A")))

       ;; A directory being walked is a directory being changed: a socket in a
       ;; temporary directory comes and goes, and a dangling symlink is a stat
       ;; that fails.  A linter pointed at a project must say nothing about
       ;; them rather than raise.
       (test "a directory operand skips entries it cannot ask about"
         (with-temp-directory
            (lambda (directory)
              (spit (string-append directory "/real.scm")
                "(define (f x)   \n  x)\n")
              (symlink (string-append directory "/nowhere.scm")
                (string-append directory "/ghost.scm"))
              (let ((result (run-sift (list "lint" directory)
                              #:directory directory)))
                (assert-status #:expect 1 #:got result)
                (assert-contains #:expect (cadr result) #:got "real.scm"
                  #:name "the file that could be read was read"))))

         (test "fix refuses a file that does not parse, and says why"
           (with-temp-directory
              (lambda (directory)
                (let ((path (string-append directory "/broken.scm"))
                      (before "(define (f x)\n  (a b\n"))
                  (spit path before)
                  (let ((result (run-sift (list "fix" path))))
                    (assert-status #:expect 2 #:got result)
                    (assert-contains #:expect (caddr result) #:got "parse error")
                    (assert-same-text #:expect before #:got (slurp path)
                      #:name "the file is left exactly as it was"))))))

         (test "fix --force fixes what it can read"
           (with-temp-directory
              (lambda (directory)
                (let ((path (string-append directory "/broken.scm")))
                  (spit path "(define (f x)   \n  (a b\n")
                  (let ((result (run-sift (list "fix" "--force" path))))
                    (assert-status #:expect 1 #:got result)
                    (assert-contains #:expect (slurp path)
                      #:got "(define (f x)\n"
                      #:name "the trailing whitespace is gone"))))))

         (test "fix --stdin refuses a buffer that does not parse"
           (let ((result (run-sift '("fix" "--stdin" "--dialect=scheme")
                           #:stdin "(define (f x)\n  (a b\n")))
             (assert-status #:expect 2 #:got result)
             (assert-same-text #:expect "" #:got (cadr result)
               #:name "no text is written to standard output")))

         (test "lint --format=github writes workflow commands"
           (let* ((result (run-sift '("lint" "--format=github"
                                      "tests/fixtures/lint/dirty.scm")))
                  (output (cadr result)))
             (assert-status #:expect 1 #:got result)
             (assert-contains #:expect output #:got "::warning file=")
             (assert-contains #:expect output #:got "title=SIFT001 [trailing-whitespace]")))

         (test "lint --format=sarif writes a SARIF document"
           (let* ((result (run-sift '("lint" "--format=sarif"
                                      "tests/fixtures/lint/dirty.scm")))
                  (output (cadr result)))
             (assert-status #:expect 1 #:got result)
             (assert-contains #:expect output #:got "\"version\":\"2.1.0\"")
             (assert-contains #:expect output #:got "\"rules\":")
             (assert-contains #:expect output #:got "\"ruleId\":\"SIFT001\"")
             (assert-contains #:expect output #:got "\"artifactLocation\"")))

         ;; SARIF is read by a service, not by a person, so a field it is
         ;; missing is a field nobody sees: a rule with no help, an artifact
         ;; every result names in full, a region that cannot say how long the
         ;; finding is, a result that cannot be matched with last week's.
         (test "lint --format=sarif describes the rules, the artifacts and the run"
           (let* ((result (run-sift '("lint" "--format=sarif"
                                      "tests/fixtures/lint/dirty.scm")))
                  (output (cadr result)))
             (assert-status #:expect 1 #:got result)
             (for-each
                (lambda (field)
                  (assert-contains #:expect output #:got field
                    #:name (format #f "the document carries ~a" field)))
                '("\"fullDescription\":"
                  "\"help\":{\"text\":"
                  "\"helpUri\":"
                  "\"shortDescription\":"
                  "\"invocations\":[{\"executionSuccessful\":true}]"
                  "\"artifacts\":[{\"location\":{\"uri\":\"tests/fixtures/lint/dirty.scm\"}"
                  "\"roles\":[\"analysisTarget\"]"
                  "\"index\":0"
                  "\"columnKind\":\"unicodeCodePoints\""
                  "\"partialFingerprints\":{\"siftFingerprint/v1\":"
                  "\"charOffset\":"
                  "\"charLength\":"
                  "\"ruleIndex\":0"))
             (assert-contains #:expect output #:got "\"ruleId\":\"SIFT001\""
               #:name "the rule is named by its code")))

         (test "a document format writes one document for the whole run"
           (let* ((result (run-sift '("ci" "--format=sarif"
                                      "tests/fixtures/lint/dirty.scm"
                                      "tests/fixtures/scheme/community/input/messy.scm")))
                  (output (cadr result)))
             (assert-status #:expect 1 #:got result)
             ;; two files, one document: a second `$schema' would be a second
             ;; document, which is what a code scanning service cannot read
             (assert-equal #:expect 1 #:got (occurrences "\"$schema\"" output)
               #:name "there is one document")
             (assert-equal #:expect 2 #:got (occurrences "\"analysisTarget\"" output)
               #:name "both files are artifacts")
             ;; a formatting difference is a finding of its own, with a rule of
             ;; its own, so that `ci --format=sarif' says everything `ci' says
             (assert-contains #:expect output #:got "\"ruleId\":\"format\"")
             (assert-contains #:expect output #:got "\"ruleId\":\"SIFT001\"")))

         (test "a document format answers a run of several files once"
           (let* ((result (run-sift '("lint" "--format=json"
                                      "tests/fixtures/lint/dirty.scm"
                                      "tests/fixtures/lint/dirty.scm")))
                  (output (cadr result)))
             (assert-status #:expect 1 #:got result)
             (assert-true (string-prefix? "[" output) #:name "it is one array")
             (assert-equal #:expect 1 #:got (occurrences ",{\"file\"" output)
               #:name "one array holds them all")))

         (test "doctor says what ran and what it decided"
           (let ((result (run-sift '("doctor"))))
             (assert-status #:expect 0 #:got result)
             ;; the version is asserted by shape, so that bumping it is one edit
             ;; in one place rather than an edit here too
             (assert-contains #:expect (cadr result) #:got "sift "
               #:name "doctor starts with the version")
             (assert-contains #:expect (cadr result) #:got "rules")
             (assert-contains #:expect (cadr result) #:got "sift.yaml")))

         (test "ci passes on a clean file"
           (let ((result (run-sift '("ci" "tests/fixtures/lint/clean.scm"))))
             (assert-status #:expect 0 #:got result)))

         (test "ci fails when a file is not formatted"
           (let ((result (run-sift '("ci" "tests/fixtures/scheme/community/input/messy.scm"))))
             (assert-status #:expect 1 #:got result)
             (assert-contains #:expect (cadr result) #:got "@@ ")))

         (test "ci fails when the linter has something to say"
           (let ((result (run-sift '("ci" "tests/fixtures/lint/dirty.scm"))))
             (assert-status #:expect 1 #:got result)
             (assert-contains #:expect (cadr result) #:got "[trailing-whitespace]")))

         (test "ci does not write the file it is judging"
           (with-temp-directory
              (lambda (directory)
                (let ((path (string-append directory "/dirty.scm"))
                      (before "(define (f x)\n  (+ x 1))   \n"))
                  (spit path before)
                  (let ((result (run-sift (list "ci" path))))
                    (assert-status #:expect 1 #:got result)
                    (assert-same-text #:expect before #:got (slurp path)))))))

         (test "ci refuses to fix"
           (let ((result (run-sift '("ci" "--fix" "tests/fixtures/lint/clean.scm"))))
             (assert-status #:expect 2 #:got result)
             (assert-contains #:expect (caddr result) #:got "never fixes")))

         (test "ci --format=github turns formatting differences into annotations"
           (let ((result (run-sift '("ci" "--format=github"
                                     "tests/fixtures/scheme/community/input/messy.scm"))))
             (assert-status #:expect 1 #:got result)
             (assert-contains #:expect (cadr result) #:got "::error file=")))

         (test "install-hooks writes a pre-commit hook"
           (with-temp-directory
              (lambda (directory)
                (let ((git (string-append directory "/.git")))
                  (mkdir git)
                  (let ((result (run-sift '("install-hooks") #:directory directory)))
                    (assert-status #:expect 0 #:got result)
                    (let ((hook (string-append git "/hooks/pre-commit")))
                      (assert-true (file-exists? hook)
                        #:name "the hook was written")
                      (assert-contains #:expect (slurp hook) #:got "sift")))))))

         (test "install-hooks leaves an existing hook alone"
           (with-temp-directory
              (lambda (directory)
                (let ((git (string-append directory "/.git")))
                  (spit (string-append git "/hooks/pre-commit") "#!/bin/sh\ntrue\n")
                  (let ((result (run-sift '("install-hooks") #:directory directory)))
                    (assert-status #:expect 2 #:got result)
                    (assert-contains #:expect (caddr result) #:got "already exists"))))))

         (test "install-hooks --force replaces one"
           (with-temp-directory
              (lambda (directory)
                (let ((git (string-append directory "/.git")))
                  (spit (string-append git "/hooks/pre-commit") "#!/bin/sh\ntrue\n")
                  (let ((result (run-sift '("install-hooks" "--force")
                                  #:directory directory)))
                    (assert-status #:expect 0 #:got result)
                    (assert-contains #:expect
                      (slurp (string-append git "/hooks/pre-commit"))
                      #:got "sift"))))))

         (test "install-hooks outside a repository is a usage error"
           (with-temp-directory
              (lambda (directory)
                (let ((result (run-sift '("install-hooks") #:directory directory)))
                  (assert-status #:expect 2 #:got result)
                  (assert-contains #:expect (caddr result) #:got "no git repository")))))

         (test "list-checkers prints a table"
           (let ((result (run-sift '("list-checkers"))))
             (assert-status #:expect 0 #:got result)
             (assert-contains #:expect (cadr result) #:got "trailing-whitespace")
             (assert-contains #:expect (cadr result) #:got "SEVERITY")))

         (test "list-checkers --dialect filters to the dialect"
           (let ((result (run-sift '("list-checkers" "--dialect=elisp"))))
             (assert-status #:expect 0 #:got result)
             (assert-contains #:expect (cadr result) #:got "docstring-missing")
             (assert-not-contains #:expect (cadr result) #:got "internal-define")))

         (test "explain documents a checker"
           (let ((result (run-sift '("explain" "if-without-else"))))
             (assert-status #:expect 0 #:got result)
             (assert-contains #:expect (cadr result) #:got "DESCRIPTION")
             (assert-contains #:expect (cadr result) #:got "DIALECTS")))

         (test "explain answers 2 for a checker it does not know"
           (let ((result (run-sift '("explain" "no-such-checker"))))
             (assert-status #:expect 2 #:got result)
             (assert-contains #:expect (caddr result) #:got "unknown checker")))

         (test "config show prints the effective configuration"
           (let ((result (run-sift '("config" "show"))))
             (assert-status #:expect 0 #:got result)
             (assert-contains #:expect (cadr result) #:got "line-length")
             (assert-contains #:expect (cadr result) #:got "style")))

         (test "config explain documents a key"
           (let ((result (run-sift '("config" "explain" "line-length"))))
             (assert-status #:expect 0 #:got result)
             (assert-contains #:expect (cadr result) #:got "--line-length"))))

       ;; The configuration a project can actually write in its own file.  These
       ;; are the two settings the README documents in YAML, and a YAML reader can
       ;; only give them if it reads a two-element list as a pair and an empty flow
       ;; sequence as an empty list.  Both were unreadable: `indent-rules' was
       ;; rejected as "not a list of pairs" however it was written, and `[]' became
       ;; a list holding one empty string.
       (suite "project configuration"
         #:concurrent? #f

         (test "indent-rules written as a YAML sequence indents the macro"
           (with-temp-directory
              (lambda (directory)
                (spit (string-append directory "/sift.yaml")
                  "indent-rules:\n  - [with-my-thing, 2]\n")
                (spit (string-append directory "/macro.scm")
                  "(with-my-thing\n(a)\n(b))\n")
                (let ((result (run-sift '("format" "macro.scm") #:directory directory)))
                  (assert-status #:expect 0 #:got result)
                  (assert-same-text #:expect "(with-my-thing\n  (a)\n  (b))\n"
                    #:got (cadr result))))))

         (test "an empty flow sequence is an empty list of names"
           (with-temp-directory
              (lambda (directory)
                (spit (string-append directory "/sift.yaml") "exclude-checkers: []\n")
                (let ((result (run-sift '("config" "show") #:directory directory)))
                  (assert-status #:expect 0 #:got result)
                  (assert-contains #:expect (cadr result)
                    #:got "exclude-checkers: []")))))))))
