;; sift --- command line tests
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

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

(define (spec)
  (suite "cli"

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
        (assert-contains #:expect (cadr result) #:got "--line-length")))))
