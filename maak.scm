;; sift --- a dialect-aware formatter and linter for Scheme, Emacs Lisp and
;; Common Lisp.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  ;; `run' and `test' are both task names here, and both shadow something the
  ;; maak DSL exports, so the DSL is imported under a prefix.
  #:use-module ((maak dsl) #:prefix dsl:)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-13)
  #:export (test
             check
             install
             uninstall
             fixture-check
             self-check
             emacs-test
             emacs-check
             docs
             complexity
             repl
             fmt
             clean
             default))

;; ====== Constants ======

(define project-name
  'sift)

;; Where the tests' veritas dependency lives.  The manual documents this:
;; VERITAS_DIR wins, ~/Hacking/veritas is the default, and vendor/veritas is
;; used when it has been checked out as a submodule.
(define (veritas-directory)
  (or (getenv "VERITAS_DIR")
    (and (file-exists? "vendor/veritas/src/veritas/veritas.scm")
      "vendor/veritas")
    (format #f "~a/Hacking/veritas" (home-dir))))

(define (veritas-source)
  (format #f "~a/src" (veritas-directory)))

(define %fixture-check-script
  "set -eu
for dir in tests/fixtures/*/*/expected; do
  style=$(basename \"$(dirname \"$dir\")\")
  case $(basename \"$(dirname \"$(dirname \"$dir\")\")\") in
    elisp) dialect=elisp ;;
    cl) dialect=cl ;;
    *) dialect=scheme ;;
  esac
  ./bin/sift check --dialect=\"$dialect\" --style=\"$style\" \"$dir\"/*
done")

;; ====== Helpers ======

(define (guile-load-flags)
  "The load path every target uses: this project's library and its tests."
  (list "-L" "./src" "-L" "." "-L" (veritas-source)))

(define (run-guile code)
  "Run CODE, a single Scheme expression, under the manifest's Guile.

The expression is one shell word, so it keeps its own quoting inside the
token while the rest of the command line stays arguments."
  (dsl:manifest-shell
     (append (list "guile") (guile-load-flags)
       (list "-c" (format #f "'~a'" code)))
     #:verbose? #t))

;; ====== Tasks ======

(define (test)
  "Run the whole Veritas suite.

Veritas is the only test framework this project uses; it is loaded from
VERITAS_DIR (~/Hacking/veritas by default) so that the checkout under test is
the checkout that is being developed, and the load path is passed here rather
than exported by the caller, so `maak test' works from a clean checkout."
  (run-guile "(begin (use-modules (tests run)) (exit (run-all!)))"))

(define (check)
  "Compile every source and test module, so a syntax error fails the build.

`sift check' over the fixture corpus runs inside the suite as well, but
compiling catches a broken module the suite may never load.  The script is
handed to `sh' as one quoted word: an unquoted one would be split at its
first semicolon and run as three commands."
  (dsl:manifest-shell
     (list "sh" "-c"
       (dsl:shell-quote
          (format #f "set -eu\nexport GUILE_LOAD_PATH='./src:.:~a:$GUILE_LOAD_PATH'\nfind src tests -name '*.scm' -not -path 'tests/fixtures/*' -print0 | xargs -0 -n 1 sh -c 'guild compile -o \"$(mktemp)\" \"$1\"' sh"
            (veritas-source))))
     #:verbose? #t))

(define (fixture-check)
  "Check that every fixture in the corpus is already formatted.

This is the check a CI job runs over real code: the expected files of the
corpus must be byte-for-byte what ~sift format~ produces.  The dialect and the
style are read from the fixture's path, because the corpus is filed by both."
  (dsl:manifest-shell
     (list "sh" "-c"
       (dsl:shell-quote
          %fixture-check-script))
     #:verbose? #t))

(define (self-check)
  "Check that sift's own sources are formatted by sift.

A formatter that cannot read its own project is not one to trust with anyone
else's; this is also the cheapest end-to-end test there is, since it runs the
real executable over every module in the tree."
  (dsl:manifest-shell
     (list "sh" "-c"
       (dsl:shell-quote
          "set -eu\nfind src tests -name '*.scm' -not -path 'tests/fixtures/*' -print0 | xargs -0 ./bin/sift check"))
     #:verbose? #t))

(define (docs)
  "Regenerate doc/rules.org from the checker records.

The catalogue is not written by hand: a rule that exists is in the document,
and the test suite fails when the two disagree."
  (run-guile "(use-modules (sift catalogue)) (call-with-output-file catalogue-file (lambda (port) (display (catalogue->org) port)))"))

(define (complexity . args)
  "Report how hard each definition in the tree is to read.

Refactoring then has a target rather than an opinion: `etc/complexity.scm'
reads every definition with sift's own reader and prints its cognitive
complexity, its cyclomatic complexity, its depth and its length, worst first.

The words after the task name are the paths to report on, so `maak complexity
tests' ranks the test suite instead; `SIFT_COMPLEXITY_PATHS' does the same
from the environment.  `SIFT_COMPLEXITY_BUDGET' and `SIFT_COMPLEXITY_LIMIT'
move the budget and the length of the table, because a task's arguments are
its operands and a flag cannot be passed through this way."
  (dsl:manifest-shell
     (append (list "guile" "-L" "./src" "-L" "." "etc/complexity.scm")
       (if (null? args) (list "src") args))
     #:verbose? #t))

(define (emacs-test)
  "Run the Emacs package's ERT suite.

ERT is the test framework Emacs ships, and it is the right one here: the
package is a thin wrapper around the executable, so the tests run the
executable and read what came back, the way a user would.  The Guile suite
is Veritas; this one never runs the two together."
  (dsl:manifest-shell
     (list "emacs" "-Q" "--batch" "-L" "emacs" "-l" "ert"
       "-l" "emacs/sift-test.el" "-f" "ert-run-tests-batch-and-exit")
     #:verbose? #t))

(define (emacs-check)
  "Byte-compile the Emacs package, with warnings treated as errors.

A package that compiles with a warning about a free variable or a wrong
number of arguments is a package that will misbehave on someone else's
Emacs; the compiled file is removed afterwards so that the tests always load
the source under test."
  (dsl:manifest-shell
     (list "sh" "-c"
       (dsl:shell-quote
          "set -eu\nemacs -Q --batch --eval '(setq byte-compile-error-on-warn t)' -L emacs -f batch-byte-compile emacs/sift.el\nrm -f emacs/sift.elc"))
     #:verbose? #t))

(define (install)
  "Install the library and the launcher under $(prefix).

The launcher finds its modules relative to itself, so the tree is installed
rather than only copied: bin/sift lands in $(prefix)/bin and the library in
$(prefix)/share/guile/site/3.0."
  (let* ((prefix (or (getenv "PREFIX") "/usr/local"))
         (site (format #f "~a/share/guile/site/3.0" prefix))
         (bin (format #f "~a/bin" prefix)))
    (dsl:run (list "install" "-d" site bin) #:sudo? (needs-sudo? prefix)
      #:verbose? #t)
    (dsl:run (list "cp" "-r" "src/sift" site)
      #:sudo? (needs-sudo? prefix) #:verbose? #t)
    (dsl:run (list "install" "-m" "755" "bin/sift" bin)
      #:sudo? (needs-sudo? prefix) #:verbose? #t)))

(define (needs-sudo? prefix)
  (not (or (string-prefix? (home-dir) prefix)
         (string-prefix? "/tmp" prefix))))

(define (uninstall)
  "Remove what `install' put under $(prefix)."
  (let* ((prefix (or (getenv "PREFIX") "/usr/local"))
         (site (format #f "~a/share/guile/site/3.0/sift" prefix))
         (bin (format #f "~a/bin/sift" prefix)))
    (dsl:run (list "rm" "-rf" site bin)
      #:sudo? (needs-sudo? prefix) #:verbose? #t)))

(define (repl)
  "A REPL with the library and the tests on the load path."
  (dsl:manifest-shell
     (append (list "guile") (guile-load-flags))
     #:verbose? #t))

(define (fmt)
  "Format sift's own sources with sift.

The tool formats itself: a formatter that cannot read its own project is not
one to trust with anyone else's."
  (dsl:run (list "sh" "-c"
             (dsl:shell-quote
                "set -eu\nfind src tests -name '*.scm' -not -path 'tests/fixtures/*' -print0 | xargs -0 ./bin/sift format --write"))
    #:verbose? #t))

(define (clean)
  "Remove build artefacts."
  (delete-file-recursively "reports"))

(define (default)
  "List the tasks this project defines."
  (list-tasks))
