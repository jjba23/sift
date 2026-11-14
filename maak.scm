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
  #:use-module ((ice-9 rdelim) #:select (read-line))
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
            web
            web-serve
            web-deploy
            fmt
            repl
            install
            uninstall
            clean
            package-docker
            push-docker-latest
            push-docker-tag
            release agents))

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
    racket) dialect=racket ;;
    cl) dialect=cl ;;
    *) dialect=scheme ;;
  esac
  ./bin/sift check --dialect=\"$dialect\" --style=\"$style\" \"$dir\"/*
done")

;; ====== Maak file helpers ======

(define (guile-load-flags)
  "The load path every task uses: this project's library, its tests, its browser
 layer, Veritas.

Guile searches `-L' directories in the order they are given, before the ones a
Guile installation or profile contributes, so the checkout is looked in before
any packaged copy -- which is the only way `maak test' can promise that the
framework under development is the one the suite ran against.

`./web' is on it because `(sift web)' -- the browser's facade -- lives there and
not in `./src': the core library is what `bin/sift' is, and the browser adds a
directory beside it rather than inside it."
  (list "-L" "./src" "-L" "." "-L" "./web"))

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
   (~ "set -eu\nexport GUILE_LOAD_PATH='./src:./web:.:$GUILE_LOAD_PATH'\nfind src tests benchmarks -name '*.scm' -not -path 'tests/fixtures/*' -print0 "
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

;;; --- the browser build ---
;;;
;;; Three tasks, and a fourth that is not a task: `web' builds, `web-serve'
;;; serves what was built, and `web-deploy' builds and publishes.  The build
;;; itself is `web/build.sh', which is one command on purpose -- it is the step
;;; that has to work outside Maak too, on a machine with no Guix on it, and a
;;; task that reimplemented it would be a second build to keep correct.

(define web-dist "web/dist")

;; Where the build goes.  jointhefreeworld.org serves /srv/http/<name> as
;; https://<name>/, so publishing the editor is putting a directory there: the
;; certificate, the redirect for the bare path and the `application/wasm' type
;; are the server's business, and they were settled when the directory first
;; appeared.  Nothing about the server has to be told that the page exists.
(define web-deploy-directory
  "/srv/http/jointhefreeworld.org/sift")

(define (web)
  "Build the browser build into web/dist.

`web/build.sh' is the whole of it: it finds a Hoot that works or asks Guix for
one, compiles web/main.scm to sift.wasm, stamps the page with the version and
the commit, and says what it laid out."
  (run (list "sh" "web/build.sh") #:verbose? #t))

(define* (web-serve #:optional (port "8080"))
  "Serve web/dist on localhost, to look at the page before publishing it.

This is not part of deploying and it is not a server sift grows: it is Python's
one-line static file server, which every machine has and which sends the
`application/wasm' type the module needs.  The page has to be *served* rather
than opened from the filesystem -- a file:// page may not fetch the wasm beside
it -- and that is the whole of why this task exists."
  (unless (file-exists? (~ "~a/index.html" web-dist))
    (task-error "~a is not built; run `maak web' first" #:args (list web-dist)))
  (with-directory web-dist
                  (lambda ()
                    (log-info "serving ~a at http://localhost:~a/" web-dist port)
                    (run (list "python3" "-m" "http.server" port) #:verbose? #t))))

(define (web-deploy)
  "Build the browser build and publish it to the web directory.

Deployment is a copy, the way the rest of jointhefreeworld.org is deployed: the
directory is emptied and the fresh build is put in its place, so a file this
build does not have does not linger on the server beside the one that replaced
it.  It runs where that directory is -- on the machine that serves, or in the CI
job that runs there -- and that is all it needs: no ssh, no rsync, and no server
to reload, because a file that is there is a file that is served.

The build is removed afterwards the way `maak deploy' removes the documentation
it published: what the tree keeps is the source, and a build directory left
behind is a build directory the next deploy might copy."
  (web)
  (log-info "publishing ~a to ~a" web-dist web-deploy-directory)
  (delete-file-recursively web-deploy-directory)
  (cp web-dist web-deploy-directory)
  (delete-file-recursively web-dist))

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
  (delete-file-recursively "reports")
  ;; The browser build is an artefact too: what is served is what `maak web'
  ;; writes, and a stale directory that a deploy could copy is worse than none.
  (delete-file-recursively web-dist))

;;; --- packaging ---
;;;
;;; The container image is the Dockerfile at the root, built by whichever engine
;;; is installed: `podman build' and `docker build' read the same file, and a
;;; task that insisted on one of them would be a task half the machines it runs
;;; on cannot do.
;;;
;;; `package-docker' is for the person building the image, `push-docker-latest'
;;; and `push-docker-tag' for the person publishing it -- the same two steps
;;; maak's own file takes, because a reader who knows one of these files should
;;; not have to learn the other.

(define docker-image "jjba23/sift")

(define (container-engine-script . body)
  "Return a shell script that sets @var{engine} and then runs @var{body}.

`podman' is preferred to `docker' when both are installed, which is the order
`(maak container)' uses and the order a machine with both usually means."
  (apply string-append
         "set -eu\n"
         "engine=$(command -v podman || command -v docker)\n"
         body))

(define (package-docker)
  "Build the container image from the Dockerfile, with podman or docker."
  (run-script
   (container-engine-script
    (~ "\"$engine\" build --tag ~a:latest .\n" docker-image))))

(define (push-docker-latest)
  "Push the image `package-docker' built to Docker Hub as :latest."
  (run-script
   (container-engine-script
    (~ "\"$engine\" tag ~a:latest docker.io/~a:latest\n" docker-image docker-image)
    (~ "\"$engine\" push docker.io/~a:latest\n" docker-image))))

(define (push-docker-tag)
  "Push the image as :latest and as the tag of this checkout."
  (let ((tag (git:git-describe-tag)))
    (run-script
     (container-engine-script
      (~ "\"$engine\" tag ~a:latest docker.io/~a:latest\n" docker-image docker-image)
      (~ "\"$engine\" tag ~a:latest docker.io/~a:~a\n" docker-image docker-image tag)
      (~ "\"$engine\" push docker.io/~a:latest\n" docker-image)
      (~ "\"$engine\" push docker.io/~a:~a\n" docker-image tag)))))

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

;; ====== Agent skills ======

;; AGENTS.md is generated, never edited by hand.  The section has two halves:
;; the data below, which is this project's, and the task itself, which is not.
;; Another repository copies the whole section and replaces the data - the
;; header, the pinned skills (repository, commit, path, licence) and the
;; practices - and `maak agents' writes its own AGENTS.md.  Regeneration is
;; deterministic: the same pins and the same text produce the same file, byte
;; for byte, and it needs nothing but curl.  The skill URLs assume GitHub, so a
;; skill hosted elsewhere needs a URL of its own.

(define ponytail-commit
  "96608e078b2d5ea607c5c3b8b5a54d1c1fba6846")

(define tdd-commit
  "22f0b463e7a5e579904dd6211a520321035526f7")

;; Name, repository, commit, the path the repository keeps the skill at and the
;; licence.  A skill arrives whole: trimming it means scanning for headings, and
;; such a scan cannot tell a heading from a `##' line inside a code fence.
(define agent-skills
  (list (list "ponytail"
              "DietrichGebert/ponytail"
              ponytail-commit
              "skills/ponytail/SKILL.md"
              "MIT")
        (list "test-driven-development"
              "obra/superpowers"
              tdd-commit
              "skills/test-driven-development/SKILL.md"
              "MIT")))

(define agent-skills-dir
  "reports/agent-skills")

(define agents-header
  "# AGENTS.md

Generated by `maak agents` from the pinned skills and the practices in
`maak.scm`; edit that task, never this file.  The pins and the text are the whole
input, so regenerating gives the same document.  The repository's own rules win
over general advice, and the narrowest rule here wins over the broadest.
")

;; Practices this project keeps.  They are short on purpose: a rule a reader
;; checks in a second is a rule that gets followed.  The pinned skills are the
;; long form of the same habits.
(define agent-practices
  (list
   "## Documentation

- Every user-visible change reaches the documentation: the page that owns the
  feature, the manual page for a flag or an environment variable, and the
  release notes under the unreleased heading.
- Keep the table of contents in step with the headings.  A section with no entry
  is a section nobody finds.
- The guide is Org: one `*` heading per page, `**` for sections, `#+TITLE` and
  `#+EMAIL` at the top, a `CUSTOM_ID` on anything linked from elsewhere, and
  `file:` links between pages.
- Explain why and when, never what: the code already says what.  A docstring's
  first line works as a summary on its own.
- Spell an identifier the way the code spells it - the exact name, not a
  description of it.
- An example must run as written, copied from a real invocation.
- No promises about the future and no changelog in the guide; history lives in
  the release notes.
"
   "## Security

- No secrets in the tree.  Tokens, keys and passwords arrive through the
  environment, and no recipe, workflow or example embeds one.
- Validate at the edge: anything from a file, a report, a path or a subprocess
  is untrusted until it has been checked.
- Never build a shell string out of data.  Tasks pass argument lists, and Guile
  calls `(system \"program\" \"arg\")` or `open-pipe*`, never a command
  assembled with `string-append`.
- Do not `eval` or `read` input you did not write; parse with `(json)` or a
  reader built for the format.
- Escape what leaves the process: a name that came from input is escaped when
  it is written to XML, HTML or a data file.
- Fail closed.  A file that cannot be read is an error, not an empty result.
- Input never chooses the path of a written file; writes stay inside the
  directory reserved for them.
- A dependency is a security decision: it must be needed, MIT compatible and
  pinned.
- Report a vulnerability privately, not in a public issue.
"
   "## Performance

- Measure first.  A before and after belongs in the benchmark, and a speedup
  claimed without a number is not a speedup.
- Prefer not doing the work: cache it, skip it, batch it, or delete the call,
  before making the call faster.
- Then the algorithm, then the data structure (hash table, vector), then the
  micro-optimisation.  In that order, for that reason.
- Guile specifics: keep module resolution and long `assoc` chains out of loops,
  use `#:declarative?` modules and compiled `.go` files at startup, and reach
  for `statprof` instead of guessing where the time goes.
- A shortcut that buys speed costs clarity; it gets a measurement and a
  `shortcut:` comment saying what it costs and when to revisit it.
- Coverage instrumentation is slow by design: iterate without it, and measure
  before finishing.
"
   "## Architecture decisions

- A decision that is expensive to reverse gets a short record: the context, the
  decision, the consequences, and the alternatives that lost.  A small decision
  belongs in the commit body instead.
- Two paragraphs beat a design document; record the choice while it is still a
  choice, not after it has become a fact.
- An accepted record is not rewritten.  Supersede it with a new one and link the
  two directions.
- The record lives next to its subject: the documentation for anything a user
  would notice, the release notes for the release-level result.
- Changes to the public API, a file format, the pipeline or the licence get this
  treatment at a minimum in the commit body.
"
   "## Semantic versioning

- The project follows SemVer 2.0.0 for its public API: the exported functions,
  the CLI flags and environment variables, and the shape of its file formats.
- A breaking change bumps MAJOR, a new feature MINOR, a fix PATCH.  A new export
  is MINOR; removing or changing one is MAJOR.
- One version string, named everywhere it appears: the source, the build
  recipes, the manual page and the packaging.
- A release is a dated release-notes section, an annotated `vX.Y.Z` tag on the
  release commit, and a packaging checksum updated afterwards from the published
  archive.
"
   "## Conventional commits

- `type(scope): summary`, imperative, lower case, no full stop, about seventy
  characters.
- Types in use: `feat`, `fix`, `perf`, `refactor`, `test`, `docs`, `build`, `ci`,
  `chore`.  A `!` after the type or a `BREAKING CHANGE:` footer marks a MAJOR
  change, and says what breaks.
- The subject says what changed.  The body says why, and what was rejected.
- One logical change per commit: a refactor and a fix travel apart, so either
  can be reverted alone.
- Run the full validation task before committing, and never commit a red suite
  without saying so in the message.
- Reference the issue in the footer - `Closes #12` - not in the subject.
"
   "## Functional programming and immutability

- Pure functions in the core, effects at the edges.  Whatever touches the file
  system or the clock sits at the edge; the core does not.
- Prefer `let` and new values to `set!`.  Build results with `map`, `filter`,
  `fold` and named `let`, not with index loops that push into a list.
- A record is a value: construct a new one instead of mutating a shared one, and
  pass configuration as arguments or parameters rather than as globals.
- Handle an exception at the boundary: catch it there, turn it into a result
  record, and let the caller decide what it means.
- A function that both computes and prints is two functions.
- Temporary effects - a changed directory, a bound environment variable - go
  through `dynamic-wind`.
- Prefer the expression style: `if` and `cond` that return a value, not a branch
  that assigns one.
"
   "## Do not reinvent the wheel

- Guile's standard library first: `(ice-9 ...)` for the runtime, `(srfi srfi-1)`
  for lists, `(srfi srfi-9)` for records, `(srfi srfi-13)` for strings, `(json)`
  for JSON.  The project already depends on all of them.
- Then this codebase: read the existing modules before writing a helper, and use
  the one that is nearly right instead of adding a second one.
- A new dependency needs a reason the standard library cannot meet, a licence
  the project can carry, and a paragraph in the commit body.
- Prefer deleting a special case to writing code that handles it.
- The best diff removes lines and keeps the behaviour; the second best is a
  small diff that is obvious a year later.
"))

(define (slurp path)
  "Return the whole of @var{path} as a string."
  (call-with-input-file path
    (lambda (port)
      (let loop ((lines '())
                 (line (read-line port)))
        (if (eof-object? line)
            (string-concatenate-reverse lines)
            (loop (cons (string-append line "\n") lines)
                  (read-line port)))))))

(define (fetch-skill name repository commit path)
  "Download one skill pinned at @var{commit}, into the reports tree.

  Returns the skill's text."
  (let ((target (~ "~a/~a.md" agent-skills-dir name))
        (url (~ "https://raw.githubusercontent.com/~a/~a/~a"
                repository commit path)))
    (run (list "curl" "--fail" "--silent" "--show-error" "--location"
               "--output" target url)
         #:verbose? #t)
    (slurp target)))

(define (agents)
  "Regenerate AGENTS.md from the pinned skills and the practices."
  (run (list "mkdir" "-p" agent-skills-dir))
  ;; Every download happens before the file is opened, so a fetch that fails
  ;; leaves the previous AGENTS.md in place rather than a half-written one.
  (let ((skills (map (lambda (skill)
                       (append skill
                               (list (fetch-skill (list-ref skill 0)
                                                  (list-ref skill 1)
                                                  (list-ref skill 2)
                                                  (list-ref skill 3)))))
                     agent-skills)))
    (call-with-output-file "AGENTS.md"
      (lambda (port)
        (display agents-header port)
        (for-each (lambda (skill)
                    (format port
                            "~%## ~a~%~%`~a` at `~a`, `~a`, ~a licensed.~%~%"
                            (list-ref skill 0)
                            (list-ref skill 1)
                            (list-ref skill 2)
                            (list-ref skill 3)
                            (list-ref skill 4))
                    (display (list-ref skill 5) port))
                  skills)
        (for-each (lambda (practice)
                    (newline port)
                    (display practice port))
                  agent-practices)))))
