;; sift --- a dialect-aware formatter and linter for Scheme, Emacs Lisp and
;; Common Lisp.
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; This is sift's Maak file, and it follows the two rules the other Maak files
;; in the collection follow.
;;
;; The *exports* are the task list.  Maak runs every procedure a file defines
;; when the file declares no exports, and only the exported ones when it does --
;; so the helpers below (`run-guile', `write-version!', `needs-sudo?' and the
;; rest) stay helpers, and `maak' lists the commands rather than the plumbing.
;;
;; A *command* is run with `run', which starts a program with the words it was
;; given and never sees a shell; a *line* is run with `time-machine-manifest-shell' or `$',
;; which join their words into one command line, so anything that must survive
;; as one word -- a Scheme expression, a script -- is quoted with
;; `shell-quote'.  Which of the two a call needs is the difference between a
;; task that works and a task that works until the first space or semicolon.

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  ;; `git' is the command, and the prefix says so at every call site: a task
  ;; named `commit' or `tag' later in this file would shadow the helper it
  ;; calls, and the call would be a puzzle rather than a line of a task.
  #:use-module ((maak git) #:prefix git:)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (help
            version
            default
            test
            bench
            check
            fixture-check
            self-check
            docs
            complexity
            editors-check
            emacs-test
            emacs-check
            fmt
            repl
            install
            uninstall
            clean
            release))

;; ====== Maak file constants ======

;; The corpus of golden files, as one script: every `expected' directory is
;; judged in the dialect and the style its path names, because the corpus is
;; filed by both.
(define fixture-check-script
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

;; ====== Maak file helpers ======

(define (guile-load-flags)
  "The load path every task uses: this project's library, its tests, Veritas.

Guile searches `-L' directories in the order they are given, before the ones a
Guile installation or profile contributes, so the checkout is looked in before
any packaged copy -- which is the only way `maak test' can promise that the
framework under development is the one the suite ran against."
  (list "-L" "./src" "-L" "."))

(define (run-guile code)
  "Run CODE, one Scheme expression, under the manifest's Guile.

`time-machine-manifest-shell' joins its words into one command line, so the expression is
quoted as one word: without that, the first space in it would end the argument
and the rest would be read as more of the command."
  (time-machine-manifest-shell (append (list "guile") (guile-load-flags)
                                       (list "-c" (shell-quote code)))
                               #:verbose? #t))

(define (run-script script)
  "Run SCRIPT with `sh -c' inside the manifest shell.

Same reason as `run-guile': the shell is handed the script as one word, so the
script keeps its pipes, its `;' and its quoting instead of being split on
them."
  (time-machine-manifest-shell (list "sh" "-c" (shell-quote script)) #:verbose? #t))

;; ====== Maak file tasks ======

(define (help)
  "Display maak's help screen."
  (time-machine-program-shell (list "maak" "--help")))

(define (show-version)
  "Print the version this checkout states, and where it is quoted.

Two tasks answer with this -- `maak version' and `maak release' with nothing
after it -- and they answer from one implementation, because a report written
twice is a report that disagrees with itself."
  (let ((current (current-version)))
    (format #t "sift ~a~%~%" current)
    (format #t "Where the version is quoted:~%")
    (format #t "  ~a~%" version-file)
    (for-each
     (lambda (file)
       (for-each (lambda (shape)
                   (format #t "  ~a  (~a)~%" file (format #f shape current)))
                 (shapes-of file)))
     (quotation-files))
    (format #t "~%To release: maak release VERSION~%")))

(define (version)
  "Print the version this checkout states, and where it is quoted."
  (show-version))

(define (default)
  "List the tasks this project defines."
  (list-tasks))

(define (test)
  "Run the whole Veritas suite.

Veritas is the only test framework this project uses; it is loaded from
VERITAS_DIR (~/Hacking/veritas by default) so that the checkout under test is
the checkout that is being developed, and the load path is passed here rather
than exported by the caller, so `maak test' works from a clean checkout."
  (run-guile "(begin (use-modules (tests run)) (exit (run-all!)))"))

(define (bench . args)
  "Time sift's own work, and say what each part costs.

What is timed is what a user waits for -- read, format, lint, and the same
over the whole tree -- and then the parts those are made of, so a regression
has somewhere to point.  Veritas is the runner here too: a benchmark is a task
whose result is a `benchmark-result', which is what the reporter already knows
how to print.

The words after the task name are paths to measure instead of `src', and
`SIFT_BENCHMARK_PATHS' does the same from the environment -- a corpus named
with colons, since a task's arguments are its operands and a flag cannot be
passed through this way."
  (run-guile
   (~ "(begin (use-modules (benchmarks run)) ~a(exit (run-benchmarks!)))"
      (if (null? args)
        ""
        (~ "(setenv \"SIFT_BENCHMARK_PATHS\" ~s) " (string-join args ":"))))))

(define (check)
  "Compile every source and test module, so a syntax error fails the build.

The suite loads the modules it needs; this loads the rest, so a module nothing
imports still has to compile.  Each file is compiled on its own, which is what
makes one broken file name itself rather than a pile of them."
  (run-script
   (~ "set -eu\nexport GUILE_LOAD_PATH='./src:.:$GUILE_LOAD_PATH'\nfind src tests benchmarks -name '*.scm' -not -path 'tests/fixtures/*' -print0 "
      "| xargs -0 -n 1 sh -c 'guild compile -o \"$(mktemp)\" \"$1\"' sh")))

(define (fixture-check)
  "Check that every fixture in the corpus is already formatted.

This is the check a CI job runs over real code: the expected files of the
corpus must be byte-for-byte what `sift format' produces, and the dialect and
the style are read from the fixture's path because the corpus is filed by both."
  (run-script fixture-check-script))

(define (self-check)
  "Check that sift's own sources are formatted by sift.

A formatter that cannot read its own project is not one to trust with anyone
else's; this is also the cheapest end-to-end test there is, since it runs the
real executable over every module in the tree."
  (run-script
   "set -eu\nfind src tests benchmarks -name '*.scm' -not -path 'tests/fixtures/*' -print0 | xargs -0 ./bin/sift check"))

(define (fmt)
  "Format sift's own sources with sift.

The tool formats itself: a formatter that cannot read its own project is not
one to trust with anyone else's.

`run' starts `sh' with the script as one word already, so the script is handed
over as written -- quoting it here would make the shell read the quotes as part
of the command name and fail with \"No such file or directory\" before the first
file was read."
  (run (list "sh" "-c"
             "set -eu\nfind src tests benchmarks -name '*.scm' -not -path 'tests/fixtures/*' -print0 | xargs -0 ./bin/sift format --write")
       #:verbose? #t))

(define (docs)
  "Regenerate doc/rules.org from the checker records.

The catalogue is not written by hand: a rule that exists is in the document,
and the test suite fails when the two disagree."
  (run-guile
   "(use-modules (sift catalogue)) (call-with-output-file catalogue-file (lambda (port) (display (catalogue->org) port)))"))

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
  (time-machine-manifest-shell
   (append (list "guile" "-L" "./src" "-L" "." "etc/complexity.scm")
           (if (null? args) (list "src") args))
   #:verbose? #t))

(define (editors-check)
  "Run the Vim plugin's tests and the VS Code extension's.

Each editor's own runner is the framework: Vim executes the plugin in batch
and answers with its exit status, and Node executes the extension's
arithmetic.  Neither needs a test framework installed, which is the point --
an integration that needs a toolchain to test is an integration nobody tests.

`maak emacs-test' is the Emacs side, and Neovim is `editors/vim/neovim.lua':
configuration rather than code, run by the editor that reads it."
  (run (list "vim" "-es" "-u" "NORC" "-N" "-S"
             "editors/vim/test/sift.vim")
       #:verbose? #t)
  (run (list "node" "editors/vscode/edit.test.js") #:verbose? #t)
  (run (list "node" "--check" "editors/vscode/extension.js")
       #:verbose? #t))

(define (emacs-test)
  "Run the Emacs package's ERT suite.

ERT is the test framework Emacs ships, and it is the right one here: the
package is a thin wrapper around the executable, so the tests run the
executable and read what came back, the way a user would.  The Guile suite is
Veritas; this one never runs the two together."
  (time-machine-manifest-shell
   (list "emacs" "-Q" "--batch" "-L" "emacs" "-l" "ert"
         "-l" "emacs/sift-test.el" "-f" "ert-run-tests-batch-and-exit")
   #:verbose? #t))

(define (emacs-check)
  "Byte-compile the Emacs package, with warnings treated as errors.

A package that compiles with a warning about a free variable or a wrong
number of arguments is a package that will misbehave on someone else's Emacs;
the compiled file is removed afterwards so that the tests always load the
source under test."
  (run-script
   "set -eu\nemacs -Q --batch --eval '(setq byte-compile-error-on-warn t)' -L emacs -f batch-byte-compile emacs/sift.el\nrm -f emacs/sift.elc"))

(define (repl)
  "A REPL with the library and the tests on the load path."
  (time-machine-manifest-shell (append (list "guile") (guile-load-flags)) #:verbose? #t))

;;; --- installing ---
;;;
;;; The launcher finds its modules relative to itself, so the tree is installed
;;; rather than only copied: bin/sift lands in $(prefix)/bin and the library in
;;; $(prefix)/share/guile/site/3.0.  A prefix the user owns needs no
;;; escalation, and asking for one anyway is how a `sudo' prompt appears for a
;;; directory under the home.

(define (needs-sudo? prefix)
  "Return @code{#t} when @var{prefix} is not one this user can write."
  (not (or (string-prefix? (home-dir) prefix)
         (string-prefix? "/tmp" prefix))))

(define (install)
  "Install the library and the launcher under $(prefix)."
  (let* ((prefix (or (getenv "PREFIX") "/usr/local"))
         (site (~ "~a/share/guile/site/3.0" prefix))
         (bin (~ "~a/bin" prefix))
         (sudo? (needs-sudo? prefix)))
    (run (list "install" "-d" site bin) #:sudo? sudo? #:verbose? #t)
    (run (list "cp" "-r" "src/sift" site) #:sudo? sudo? #:verbose? #t)
    (run (list "install" "-m" "755" "bin/sift" bin) #:sudo? sudo? #:verbose? #t)))

(define (uninstall)
  "Remove what `install' put under $(prefix)."
  (let* ((prefix (or (getenv "PREFIX") "/usr/local"))
         (site (~ "~a/share/guile/site/3.0/sift" prefix))
         (bin (~ "~a/bin/sift" prefix)))
    (run (list "rm" "-rf" site bin)
         #:sudo? (needs-sudo? prefix) #:verbose? #t)))

(define (clean)
  "Remove build artefacts."
  (delete-file-recursively "reports"))

;;; --- releasing ---
;;;
;;; The version is stated once, in the library, and quoted everywhere else: the
;;; tag's spelling in a workflow sample, the `Version:' field of the Emacs
;;; package, the badge at the top of the README.  A release rewrites the number
;;; inside a shape it knows and touches nothing else -- prose that happens to
;;; contain a number is not a version, and a task that rewrote prose is a task
;;; nobody would dare run.

(define version-file
  "src/sift/records.scm")

;; Each entry is a file and the shape the number takes in it.
(define version-quotations
  '(("emacs/sift.el" . "Version: ~a")
    ("README.org" . "v~a")
    ;; The release badge is the version as a reader sees it first, and the one
    ;; quotation nobody remembers to bump: the shield URL spells it
    ;; `release-1.4.0', with no `v' for a shape to match.
    ("README.org" . "release-~a")
    ;; The `sift doctor' transcript a reader compares their own machine to, and
    ;; the name of the extension the packaging command writes: both are the
    ;; version as a reader sees it, not as a tag.
    ("README.org" . "sift ~a")
    ("doc/editors.org" . "v~a")
    ("doc/editors.org" . "sift ~a")
    ("doc/editors.org" . "sift-~a")
    (".pre-commit-hooks.yaml" . "v~a")
    ("action.yml" . "v~a")
    (".github/workflows/sift.yml" . "v~a")
    ("editors/vscode/package.json" . "\"version\": \"~a\"")))

(define (current-version)
  "Return the version `src/sift/records.scm' states."
  (let ((text (call-with-input-file version-file get-string-all)))
    (let ((found (string-match
                  "sift-version[ \t]+\"([0-9]+\\.[0-9]+\\.[0-9]+)\"" text)))
      (unless found
        (task-error "~a does not state sift-version as a version"
                    #:args (list version-file)))
      (match:substring found 1))))

(define (valid-version? version)
  "Return @code{#t} when @var{version} is numbered like 0.5.0."
  (and (string? version)
    (string-match "^[0-9]+\\.[0-9]+\\.[0-9]+$" version)
    #t))

(define (how-many needle haystack)
  "Return how many times @var{needle} appears in @var{haystack}."
  (let loop ((start 0) (count 0))
    (let ((at (string-contains haystack needle start)))
      (if at (loop (+ at (string-length needle)) (+ count 1)) count))))

(define (rewrite-file file old new shapes)
  "Write @var{new} into @var{file} wherever @var{old} is quoted, and count them.

The file is read and written whole, so a replacement that fails cannot leave
half a file behind."
  (let* ((text (call-with-input-file file get-string-all))
         (rewritten
          (fold (lambda (shape text)
                  (let ((from (format #f shape old))
                        (to (format #f shape new)))
                    (if (string-contains text from)
                      (string-replace-substring text from to)
                      text)))
                text
                shapes))
         (count (fold
                 (lambda (shape total)
                   (+ total (how-many (format #f shape new) rewritten)))
                 0
                 shapes)))
    (unless (string=? text rewritten)
      (call-with-output-file file
                             (lambda (port) (display rewritten port))))
    count))

(define (quotation-files)
  "Return the files that quote the version, each once, in the order listed."
  (delete-duplicates (map car version-quotations) string=?))

(define (shapes-of file)
  "Return the shapes the version takes in @var{file}."
  (filter-map (lambda (entry)
                (and (string=? (car entry) file) (cdr entry)))
              version-quotations))

(define (write-version! old new)
  "Write @var{new} into every place that quotes the version, and answer where."
  (let ((written '()))
    (for-each
     (lambda (file)
       (let* ((ways (if (string=? file version-file)
                      (list "\"~a\"")
                      (shapes-of file)))
              (count (and (file-exists? file)
                       (rewrite-file file old new ways))))
         (when (and count (> count 0))
           (set! written (cons (~ "~a (~a)" file count) written)))))
     (cons version-file (quotation-files)))
    (reverse written)))

(define (gates)
  "Run every check this project has, and stop the release when one fails."
  (for-each (lambda (task)
              (log-info "release: running ~a" (car task))
              ((cdr task)))
            `(("docs" . ,docs)
              ("test" . ,test)
              ("check" . ,check)
              ("self-check" . ,self-check)
              ("fixture-check" . ,fixture-check)
              ("emacs-check" . ,emacs-check)
              ("emacs-test" . ,emacs-test))))

(define (release . args)
  "Cut a release: write the version everywhere, run the gates, commit and tag.

With a version, do the whole ritual in one command: write the number into the
library and into every place that quotes it, regenerate the rule catalogue,
run every gate, commit everything tracked as `chore(release): vX.Y.Z', and tag
that commit `vX.Y.Z' with an annotation that names it.

The gates run before the commit rather than after it, so a release that would
have been broken is a tree with a new number in it and a message saying which
check said no -- never a tag that has to be taken back.  A version that is
already tagged is refused rather than retagged, and naming the version the
files already state is how a bump made by hand is released.

With no argument, say what the current version is, where it is quoted, and
what would have to be typed to release the next one -- and change nothing."
  (let* ((current (current-version))
         (requested (and (pair? args) (car args))))
    (cond
      [(null? args)
       (show-version)
       #t]
      [(not (valid-version? requested))
       (task-error "~s is not a version; a release is numbered like 0.5.0"
                   #:args (list requested))]
      [else
        (let ((tag (~ "v~a" requested)))
          (when (member tag (git:git-tags))
            (task-error "~a is already a tag; bump the version first"
                        #:args (list tag)))
          (log-info "release: ~a -> ~a" current requested)
          (let ((written (write-version! current requested)))
            (if (null? written)
              (log-info "release: nothing quoted the old version")
              (for-each
               (lambda (where)
                 (log-info "release: wrote ~a into ~a" requested where))
               written)))
          (gates)
          (git:git (list "add" "--all"))
          (let* ((message (~ "chore(release): v~a" requested))
                 (staged (string-trim-both
                          (git:git-output (list "diff" "--cached"
                                                "--name-only")))))
            ;; A tree that already says the version is a tree with nothing to
            ;; commit: tag where it stands rather than fail on an empty commit.
            (if (string-null? staged)
              (log-info "release: nothing to commit")
              (begin
                (git:git (list "commit" "--message" message))
                (log-info "release: committed `~a'" message)))
            (git:git (list "tag" "--annotate" tag "--message"
                           (~ "sift ~a" requested)))
            (log-info "release: tagged ~a" tag)
            (log-separator)
            (log-info "A tag nobody has fetched is not a release yet:~%  git push origin trunk --follow-tags"))
          #t)])))
