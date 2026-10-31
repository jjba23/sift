;; sift --- a dialect-aware formatter and linter for Scheme, Emacs Lisp and
;; Common Lisp.
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  ;; `run' and `test' are both task names here, and both shadow something the
  ;; maak DSL exports, so the DSL is imported under a prefix.
  #:use-module ((maak dsl) #:prefix dsl:)
  ;; Likewise for git: a task called `tag' or `commit' here would shadow the
  ;; helper it calls.
  #:use-module ((maak git) #:prefix git:)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (test
             check
             editors-check
             install
             uninstall
             fixture-check
             self-check
             emacs-test
             emacs-check
             docs
             complexity
             release
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

(define (editors-check)
  "Run the Vim plugin's tests and the VS Code extension's.

Each editor's own runner is the framework: Vim executes the plugin in batch
and answers with its exit status, and Node executes the extension's
arithmetic.  Neither needs a test framework installed, which is the point --
an integration that needs a toolchain to test is an integration nobody tests.

`maak emacs-test' is the Emacs side, and Neovim is `editors/vim/neovim.lua':
configuration rather than code, run by the editor that reads it."
  (dsl:run (list "vim" "-es" "-u" "NORC" "-N" "-S"
             "editors/vim/test/sift.vim")
    #:verbose? #t)
  (dsl:run (list "node" "editors/vscode/edit.test.js") #:verbose? #t)
  (dsl:run (list "node" "--check" "editors/vscode/extension.js")
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

;; ====== Releasing ======

;; The version is stated once, here, and quoted everywhere else.
(define version-file
  "src/sift/records.scm")

;; Where the version is quoted, besides that one home.  Each entry is a file
;; and the shape the number takes in it: the tag's spelling in a workflow
;; sample, the `Version:' field of the Emacs package, the line of sample output
;; a reader sees.  A release rewrites the number inside a shape it knows and
;; touches nothing else -- prose that happens to contain a number is not a
;; version, and a task that rewrote prose is a task nobody would dare run.
(define version-quotations
  '(("emacs/sift.el" . "Version: ~a")
    ("README.org" . "v~a")
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

(define (release-error message . args)
  "Stop the release with @var{message}, a format string and its arguments."
  (apply error (string-append "release: " message) args))

(define (current-version)
  "Return the version `src/sift/records.scm' states."
  (let ((text (call-with-input-file version-file get-string-all)))
    (let ((found (string-match
                    "sift-version[ \t]+\"([0-9]+\\.[0-9]+\\.[0-9]+)\"" text)))
      (unless found
        (release-error "~a does not state sift-version as a version"
          version-file))
      (match:substring found 1))))

(define (valid-version? version)
  (and (string? version)
    (string-match "^[0-9]+\\.[0-9]+\\.[0-9]+$" version)
    #t))

(define (how-many needle haystack)
  "Return how many times @var{needle} appears in @var{haystack}."
  (let loop ((start 0) (count 0))
    (let ((at (string-contains haystack needle start)))
      (if at (loop (+ at (string-length needle)) (+ count 1)) count))))

;;; Replace one spelling of the version in one file, answering how many places
;;; it rewrote.  The file is read and written whole, so a replacement that
;;; fails cannot leave half a file behind.
(define (rewrite-file file old new shapes)
  "Write @var{new} into @var{file} wherever @var{old} is quoted, and count them."
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
             (set! written (cons (format #f "~a (~a)" file count) written)))))
       (cons version-file (quotation-files)))
    (reverse written)))

(define (gates)
  "Run every check this project has, and stop the release when one fails."
  (for-each (lambda (task)
              (format #t "~&release: running ~a~%" (car task))
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

With no argument, say what the current version is, where it is quoted, and
what would have to be typed to release the next one -- and change nothing.

With a version, do the whole ritual in one command: write the number into the
library and into every place that quotes it, regenerate the rule catalogue,
run every gate, commit everything tracked as `chore(release): vX.Y.Z', and tag
that commit `vX.Y.Z' with an annotation that names it.

The gates run before the commit rather than after it, so a release that would
have been broken is a tree with a new number in it and a message saying which
check said no -- never a tag that has to be taken back.  A version that is
already tagged is refused rather than retagged, and naming the version the
files already state is how a bump made by hand is released."
  (let* ((current (current-version))
         (requested (and (pair? args) (car args))))
    (cond
      [(null? args)
       (format #t "~&sift ~a~%" current)
       (format #t "~%Where the version is quoted:~%")
       (format #t "  ~a~%" version-file)
       (for-each
          (lambda (file)
            (for-each (lambda (shape)
                        (format #t "  ~a  (~a)~%" file
                          (format #f shape current)))
              (shapes-of file)))
          (quotation-files))
       (format #t "~%To release: maak release VERSION~%")
       0]
      [(not (valid-version? requested))
       (release-error "~s is not a version; a release is numbered like 0.5.0"
         requested)]
      [else
         (let ((tag (string-append "v" requested)))
           (when (member tag (git:git-tags))
             (release-error "~a is already a tag; bump the version first" tag))
           (format #t "~&release: ~a -> ~a~%" current requested)
           (let ((written (write-version! current requested)))
             (if (null? written)
               (format #t "release: nothing quoted the old version~%")
               (for-each
                  (lambda (where)
                    (format #t "release: wrote ~a into ~a~%" requested where))
                  written)))
           (gates)
           (git:git (list "add" "--all"))
           (let* ((message (format #f "chore(release): v~a" requested))
                  (staged (string-trim-both
                             (git:git-output (list "diff" "--cached"
                                               "--name-only")))))
             ;; A tree that already says the version is a tree with nothing to
             ;; commit: tag where it stands rather than fail on an empty commit.
             (if (string-null? staged)
               (format #t "~%release: nothing to commit~%")
               (begin
                 (git:git (list "commit" "--message" message))
                 (format #t "~%release: committed `~a'~%" message)))
             (git:git (list "tag" "--annotate" tag "--message"
                        (format #f "sift ~a" requested)))
             (format #t "release: tagged ~a~%~%" tag)
             (format #t "A tag nobody has fetched is not a release yet:~%")
             (format #t "  git push origin trunk --follow-tags~%"))
           0)])))

(define (default)
  "List the tasks this project defines."
  (list-tasks))
