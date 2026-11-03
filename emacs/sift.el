;;; sift.el --- sift formatting, linting, Flymake and Eglot integration  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; Copyright (C) 2025, 2026  Josep Bigorra

;; Author: Josep Bigorra <jjbigorra@gmail.com>
;; Version: 1.19.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: languages, lisp, tools
;; URL: https://github.com/jjba23/sift

;; This file is not part of GNU Emacs.

;; License: MIT.  See the LICENSE file in the repository, or
;; <https://opensource.org/license/mit>.

;;; Commentary:

;; sift formats and lints Scheme, Emacs Lisp and Common Lisp.  This package
;; is the editor side of it, and it is deliberately thin: every decision is
;; made by the executable, and Emacs only carries the text there and the
;; findings back.
;;
;; What it gives you:
;;
;;   M-x sift-mode
;;     A minor mode with a keymap (`C-c s f' to format, `C-c s l' to lint,
;;     `C-c s x' to fix, `C-c s e' to explain the rule at point, `C-c s p'
;;     to run the whole-project check a pipeline runs) and an optional
;;     `sift-format-on-save-mode'.
;;
;;   M-x sift-flymake-mode
;;     A Flymake backend.  It runs `sift lint --format=json --stdin' and
;;     turns each finding into a diagnostic whose text starts with the
;;     rule's code (SIFT012) and whose data carries the rule's name, so that
;;     `C-c s e' on the line opens the documentation of the rule that fired.
;;     Flymake's own `code' slot only exists from Emacs 31, so the code
;;     travels in the text and in the data, where every version can read it.
;;
;;     The run is asynchronous, and the backend keeps two things about it: the
;;     process, so that the question a new keystroke replaces is cancelled
;;     rather than answered late, and a generation number, so that an answer
;;     that arrives anyway is dropped.  Flymake calls an answer to a question
;;     it is no longer asking an "obsolete report" and says so in *Messages* on
;;     every keystroke; cancelling and numbering is what stops it.
;;
;;     An Eglot server reports the same findings, so in a buffer Eglot is
;;     managing the backend takes itself out of `flymake-diagnostic-functions',
;;     and `sift-format-on-save' stands down as well: the server formats on
;;     save, and two formatters writing one buffer on one save is two round
;;     trips and two undo entries for one keystroke.
;;
;;   M-x sift-fix-buffer, M-x sift-fix-at-point
;;     Code actions.  `sift-fix-buffer' applies every fix sift has for the
;;     buffer; `sift-fix-at-point' applies only the fixes of the rule under
;;     point, which is the useful one when a rule fires twenty times and you
;;     want one of them changed.  Both go through `sift lint --fix --stdin',
;;     so neither saves the file and both are a single `undo' away.
;;
;;   M-x sift-lint
;;     Runs the linter in a compilation buffer, so `next-error' and
;;     `previous-error' navigate the findings, and `C-c s e' on a
;;     compilation line explains its rule.
;;
;;   What waits, and what does not
;;     The commands that *report* -- checking the buffer, explaining a rule --
;;     are asynchronous: an answer the next command does not depend on is not
;;     worth freezing the editor for.  Formatting and fixing are not: their
;;     result is what the save that follows writes, so `sift-format-buffer' has
;;     the fixed text in hand before it returns, and the change is one `undo'
;;     away.
;;
;; Setup, for all three dialects:
;;
;;   (add-hook 'scheme-mode-hook #'sift-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'sift-mode)
;;   (add-hook 'lisp-mode-hook #'sift-mode)
;;
;; or just `M-x sift-setup', which adds those hooks for you.
;;
;; Nothing here assumes a project layout: the dialect comes from the major
;; mode, the executable from `sift-executable', and every setting sift has
;; (line length, bracket style, which checkers) is read from the project's
;; own `sift.yaml' by the executable itself.

;;; Code:

(require 'flymake)
(require 'json)
(require 'compile)
(require 'subr-x)

;; Eglot is loaded by the user, not by this package: the Flymake backend is the
;; integration with no dependencies, and Eglot is the one with code actions.
(declare-function eglot-code-actions "eglot" (&optional arg))
(declare-function eglot-current-server "eglot" (&optional buffer))
(defvar eglot-server-programs)

(defgroup sift nil
  "Formatting and linting for Scheme, Emacs Lisp and Common Lisp."
  :group 'languages
  :prefix "sift-"
  :link '(url-link "https://github.com/jjba23/sift"))

(defcustom sift-executable "sift"
  "The sift program to run."
  :type 'string
  :group 'sift)

(defcustom sift-dialect nil
  "Dialect to tell sift about, or nil to guess from the major mode.

The value is one of \"scheme\", \"elisp\" or \"cl\".  Leave it nil unless
you edit a Lisp in a mode this package does not know: the project's own
`sift.yaml' is the right place for a project-wide setting."
  :type '(choice (const :tag "From the major mode" nil)
           (const "scheme") (const "elisp") (const "cl"))
  :group 'sift)

(defcustom sift-format-style nil
  "Formatting style to ask for, or nil to let sift decide."
  :type '(choice (const :tag "Default" nil)
           (const "community") (const "dangling"))
  :group 'sift)

(defcustom sift-comment-align nil
  "When non-nil, ask the formatter to align inline comments."
  :type 'boolean
  :group 'sift)

(defcustom sift-format-on-save nil
  "When non-nil, `sift-mode' formats the buffer before saving it."
  :type 'boolean
  :group 'sift)

(defcustom sift-lint-severity "info"
  "Minimum severity the linter reports, or nil to let sift decide.

A value here is passed on every run and therefore overrides the severity the
project sets in its own `sift.yaml'; nil passes nothing, and the project is
the one that decides.  \"info\" is everything sift has, which is what a
buffer wants while it is being written."
  :type '(choice (const :tag "The project's own setting" nil)
           (const "error") (const "warning") (const "info"))
  :group 'sift)

(defcustom sift-extra-arguments nil
  "Extra arguments to pass to every sift invocation."
  :type '(repeat string)
  :group 'sift)

(defconst sift--mode-dialects
  '((scheme-mode . "scheme")
    (scheme-ts-mode . "scheme")
    (emacs-lisp-mode . "elisp")
    (lisp-mode . "cl")
    (lisp-data-mode . "cl")
    (clojure-mode . nil))
  "Dialect to use for each major mode that sift knows about.")

;;; Running sift

(defun sift--add-mode-hook (mode function)
  "Add FUNCTION to MODE's hook, defining the hook when it does not exist yet.

A mode that has not been loaded has no hook variable, and `add-hook' signals
on a variable that is not bound: setting it to nil first is what lets an init
file call `sift-setup' before Scheme or Lisp mode has ever been opened."
  (let ((hook (intern (format "%s-hook" mode))))
    (unless (boundp hook)
      (set-default hook nil))
    (add-hook hook function)))

(defun sift--dialect ()
  "Return the dialect of the current buffer, as a string, or nil."
  (or sift-dialect
    (cdr (assq major-mode sift--mode-dialects))))

(defun sift--require-dialect ()
  "Return the dialect of the buffer, or signal a user error.

sift cannot read a Lisp without being told which Lisp it is, and guessing
from the text is exactly what its `--dialect' option exists to avoid."
  (or (sift--dialect)
    (user-error "sift: no dialect for `%s'; set `sift-dialect'" major-mode)))

(defun sift--label ()
  "Return the name sift should use for the current buffer."
  (or (buffer-file-name) (buffer-name)))

(defun sift--options ()
  "Return the arguments every invocation shares."
  (append
     (when sift-format-style (list (concat "--style=" sift-format-style)))
     (when sift-comment-align (list "--comment-align"))
     sift-extra-arguments))

(defun sift--severity-options ()
  "Return the severity argument, or nothing when the project decides.

`sift-lint-severity' is passed as `--severity' rather than written into each
command, so that the one setting means the same thing everywhere and nil
means the same thing everywhere too: sift is told nothing and reads the
threshold the project set for itself."
  (when sift-lint-severity
    (list (concat "--severity=" sift-lint-severity))))

(defun sift--process-environment ()
  "Return the environment sift runs in."
  (list (concat "NO_COLOR=1")))

(defun sift--run (arguments &optional source)
  "Run sift with ARGUMENTS, sending SOURCE to its standard input.

Return a plist with `:status', `:stdout' and `:stderr'.  This call waits:
it is used by the commands a person types, where a few dozen milliseconds
are invisible, and not by Flymake."
  (with-temp-buffer
    (when source (insert source))
    (let* ((error-file (make-temp-file "sift-stderr-"))
           (status
              (apply #'call-process-region (point-min) (point-max)
                sift-executable
                ;; the buffer holds the input: empty it, and let sift's
                ;; output land in its place
                t
                (list t error-file)
                nil
                (append arguments (sift--options))))
           (stdout (buffer-string))
           (stderr (with-temp-buffer
                     (insert-file-contents error-file)
                     (buffer-string))))
      (delete-file error-file)
      (list :status status :stdout stdout :stderr stderr))))

(defun sift--run-async (arguments source callback)
  "Run sift with ARGUMENTS on SOURCE, calling CALLBACK when it exits.

CALLBACK receives the same plist `sift--run' returns.  The caller is given the
process back as soon as it starts, so a command that waits for the answer can
say so in the echo area while it waits; Flymake uses the process to cancel a
question that a newer one has replaced.

The two buffers the answer is read from are kept on the process, so that a run
that is cancelled -- rather than finishing -- takes them with it."
  (let* ((stdout-buffer (generate-new-buffer " *sift-stdout*"))
         (stderr-buffer (generate-new-buffer " *sift-stderr*"))
         (process-environment (append (sift--process-environment)
                                process-environment))
         (process
            (make-process
                          :name "sift"
                          :buffer stdout-buffer
                          :stderr stderr-buffer
                          :command (cons sift-executable
                                     (append arguments (sift--options)))
                          :coding 'utf-8-unix
                          :connection-type 'pipe
                          :noquery t
                          :sentinel
                          (lambda (proc _event)
                            ;; a process that was cancelled is not an answer:
                            ;; only one that exited, or died, has one
                            (let ((answered? (memq (process-status proc)
                                                   '(exit signal)))
                                  (status (process-exit-status proc))
                                  (stdout (with-current-buffer stdout-buffer
                                            (buffer-string)))
                                  (stderr (with-current-buffer stderr-buffer
                                            (buffer-string))))
                              (sift--discard-buffers proc)
                              (when answered?
                                (funcall callback
                                  (list :status status :stdout stdout
                                        :stderr stderr))))))))
    (process-put process 'sift-buffers (cons stdout-buffer stderr-buffer))
    (process-send-string process source)
    (process-send-eof process)
    process))

(defun sift--discard-buffers (process)
  "Kill the buffers PROCESS was writing its answer into."
  (let ((buffers (process-get process 'sift-buffers)))
    (process-put process 'sift-buffers nil)
    (when (consp buffers)
      (when (buffer-live-p (car buffers)) (kill-buffer (car buffers)))
      (when (buffer-live-p (cdr buffers)) (kill-buffer (cdr buffers))))))

(defun sift--cancel (process)
  "Stop PROCESS, if it is still running, and forget what it was writing.

Flymake asks its backend again on every change, and an answer about text that
is no longer in the buffer is worse than no answer: it moves the squiggles back
to where they used to be.  Cancelling the process is how the previous question
is withdrawn, and it is cheaper than letting it finish and throwing the answer
away."
  (when (and process (process-live-p process))
    (delete-process process))
  (when process
    ;; a killed process runs its sentinel, which does this too; a process that
    ;; was already gone may not have been given the chance
    (sift--discard-buffers process)))

(defun sift--failure (result)
  "Return a message for a run that failed, or nil."
  (let ((status (plist-get result :status)))
    (cond
      ((= status 0) nil)
      ((= status 1) nil) ; findings, not a failure
      ((= status 2)
       (string-trim (plist-get result :stderr)))
      (t (format "sift exited %s: %s" status
           (string-trim (plist-get result :stderr)))))))

;;; Formatting

(defun sift--format-string (text)
  "Return TEXT formatted with sift.

A run sift could not finish -- a buffer with a parse error, a dialect it does
not know, an executable that is not there -- is a `user-error' with the
reason, never nil: a formatter that silently does nothing teaches people to
believe it worked."
  (let* ((dialect (sift--require-dialect))
         (result (sift--run
                    (append (list "format" "--stdin" "--stdout"
                              (concat "--label=" (sift--label))
                              (concat "--dialect=" dialect)))
                    text)))
    (if (sift--failure result)
      (user-error "sift: %s" (sift--failure result))
      (plist-get result :stdout))))

(defun sift-format-buffer ()
  "Format the whole buffer with sift.

The buffer text is sent to the executable and the answer replaces it, so
nothing is written to disk and the change is one `undo' away.  The point is
kept where it was, unless the formatting changed the text before it."
  (interactive)
  (let ((formatted (sift--format-string
                      (buffer-substring-no-properties (point-min) (point-max)))))
    (cond
      ((string= formatted (buffer-substring-no-properties
                             (point-min) (point-max)))
       (message "sift: already formatted"))
      (t
         (let ((point (point)))
           (atomic-change-group
              (let ((inhibit-modification-hooks nil))
                (erase-buffer)
                (insert formatted)))
           (goto-char (min point (point-max)))
           (message "sift: formatted"))))))

(defun sift-format-region (beg end)
  "Format the region between BEG and END with sift.

A region is formatted as the forms it contains, so a region that cuts a
form in half is a parse error rather than a silent change; sift says so and
the buffer is left alone."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (formatted (sift--format-string text)))
    (cond
      ((string= formatted text) (message "sift: already formatted"))
      (t
         (atomic-change-group
            (delete-region beg end)
            (goto-char beg)
            (insert formatted))
         (message "sift: formatted the region")))))

;;; Fixing

(defun sift--fix-string (text &optional checkers)
  "Return TEXT with sift's fixes applied, or TEXT itself when there are none.

A run sift could not finish is a `user-error', for the reason
`sift--format-string' gives: saying nothing to fix about a file that does not
parse is a lie.

CHECKERS, when given, restricts the fixes to those rules, which is what
makes a per-diagnostic code action possible."
  (let* ((dialect (sift--require-dialect))
         (result (sift--run
                    (append (list "lint" "--fix" "--stdin" "--format=null"
                              (concat "--label=" (sift--label))
                              (concat "--dialect=" dialect))
                      (mapcan (lambda (checker)
                                (list (concat "--checker=" (symbol-name checker))))
                        checkers))
                    text)))
    (if (sift--failure result)
      (user-error "sift: %s" (sift--failure result))
      (plist-get result :stdout))))

(defun sift--replace-buffer (text what)
  "Replace the buffer with TEXT, saying WHAT was done."
  (let ((point (point)))
    (atomic-change-group
       (erase-buffer)
       (insert text))
    (goto-char (min point (point-max)))
    (message "sift: %s" what)))

(defun sift-fix-buffer ()
  "Apply every fix sift has for the buffer.

This is the whole-buffer code action; `sift-fix-at-point' applies the fixes
of one rule only."
  (interactive)
  (let* ((source (buffer-substring-no-properties (point-min) (point-max)))
         (fixed (sift--fix-string source)))
    (cond
      ((string= fixed source) (message "sift: nothing to fix"))
      (t (sift--replace-buffer fixed "fixed")))))

(defun sift--diagnostic-at-point ()
  "Return the Flymake diagnostic at point, or nil.

Outside `flymake-mode' there is nothing to ask, and a buffer that linted
itself with a compilation buffer is not supposed to fail here either."
  (when (and (fboundp 'flymake-diagnostics) (bound-and-true-p flymake-mode))
    (ignore-errors (car (flymake-diagnostics (point))))))

(defun sift--diagnostic-checker (diagnostic)
  "Return the checker a Flymake DIAGNOSTIC came from."
  (let ((data (and diagnostic (flymake-diagnostic-data diagnostic))))
    (plist-get data :checker)))

(defun sift-fix-at-point ()
  "Apply the fixes of the rule reported at point.

A rule can fire twenty times in a buffer; this is the code action that
changes the one you are looking at, and `sift-fix-buffer' is the one that
changes them all."
  (interactive)
  (let ((checker (sift--diagnostic-checker (sift--diagnostic-at-point))))
    (if (not checker)
      (user-error "No sift diagnostic at point")
      (let* ((source (buffer-substring-no-properties (point-min) (point-max)))
             (fixed (sift--fix-string source (list checker))))
        (cond
          ((null fixed) nil)
          ((string= fixed source)
           (message "sift: `%s` has no fix" checker))
          (t (sift--replace-buffer fixed
               (format "fixed `%s`" checker))))))))

;;; Checking and linting

(defun sift-check-buffer ()
  "Ask sift whether the buffer is formatted, and say so.

The run is asynchronous and the answer is a message: a question about
formatting is not something the next command depends on, so waiting for it
would only be a way of freezing the editor for the length of one process."
  (interactive)
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (dialect (sift--require-dialect)))
    (message "sift: checking…")
    (sift--run-async
     (append (list "check" "--stdin"
                   (concat "--label=" (sift--label))
                   (concat "--dialect=" dialect)))
     source
     (lambda (result)
       (let ((failure (sift--failure result)))
         (cond
           (failure (message "sift: %s" failure))
           ((= 0 (plist-get result :status))
            (message "sift: formatted"))
           (t
            (message "sift: this buffer would be reformatted (M-x sift-format-buffer)"))))))))

(defconst sift--error-regexp
  "^\\(?:[0-9]+:\\)?\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\): \\(error\\|warning\\|info\\): \\[\\([^]]+\\)\\] \\(.*\\)$"
  "Regexp matching one line of sift's human-readable output.")

(add-to-list 'compilation-error-regexp-alist-alist
  `(sift ,sift--error-regexp 1 2 3 (4 . nil) 5))
(add-to-list 'compilation-error-regexp-alist 'sift)

(defun sift-lint ()
  "Lint the current file in a compilation buffer.

`next-error' and `previous-error' walk the findings, and
`sift-explain-at-point' explains the rule on the line under point.  A
buffer that is not visiting a file should use `sift-flymake-mode' or
`sift-check-buffer' instead: sift reports positions in a file, and there is
no file to report them in."
  (interactive)
  (unless (buffer-file-name)
    (user-error "sift-lint needs a buffer visiting a file; try sift-flymake-mode"))
  (when (buffer-modified-p)
    (unless (y-or-n-p "The buffer has unsaved changes; save it first? ")
      (user-error "Not linting an unsaved buffer"))
    (save-buffer))
  (let* ((dialect (sift--dialect))
         (command (string-join
                     (append (list (shell-quote-argument sift-executable)
                               "lint")
                       (sift--severity-options)
                       (sift--options)
                       (when dialect (list (concat "--dialect=" dialect)))
                       (list (shell-quote-argument (buffer-file-name))))
                     " ")))
    (compilation-start command nil (lambda (_mode) "*sift-lint*"))))

;;; Explaining rules

(defun sift-explain (rule)
  "Show sift's documentation of RULE in a buffer.

The buffer appears at once and is filled in when the executable answers: a
documentation buffer is a place to read, not a value the next command needs,
so waiting for it would only be a way of freezing the editor."
  (interactive
    (list (completing-read "Rule: "
            (sift--rule-names) nil nil
            (sift--rule-at-point))))
  (let ((buffer (get-buffer-create "*sift-explain*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "sift: reading the documentation of %s…\n" rule))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buffer)
    (sift--run-async
     (list "explain" rule) ""
     (lambda (result)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (if (= 0 (plist-get result :status))
                       (plist-get result :stdout)
                       (string-trim (plist-get result :stderr))))
             (goto-char (point-min)))))))))

(defun sift-ci ()
  "Run `sift ci' over the project in a compilation buffer.

The same check a pipeline runs: formatting and lint for every file under
the repository root.  `next-error' walks the findings, and the exit status
is the one a CI job would see."
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory ".git")
                             default-directory)))
    (compilation-start (concat (shell-quote-argument sift-executable) " ci")
      nil (lambda (_mode) "*sift-ci*"))))

(defvar sift--rule-name-cache nil
  "The rule names last read from the executable, as (EXECUTABLE . NAMES).

The list comes from `sift list-checkers', which costs a process; it is a
property of one build of the executable and nothing else, so it is asked for
once per `sift-executable' rather than once per completion.")

(defun sift--rule-names ()
  "Return every rule name sift knows about."
  (if (and (consp sift--rule-name-cache)
        (equal sift-executable (car sift--rule-name-cache)))
    (cdr sift--rule-name-cache)
    (let ((result (sift--run (list "list-checkers"))))
      (when (= 0 (plist-get result :status))
        (let ((names (delq nil
                           (mapcar (lambda (line)
                                     (when (string-match "^\\([a-z0-9-]+\\) +SIFT[0-9]+" line)
                                       (match-string 1 line)))
                                   (split-string (plist-get result :stdout) "\n")))))
          (setq sift--rule-name-cache (cons sift-executable names))
          names)))))

(defun sift--rule-at-point ()
  "Return the name of the rule named on this line, or nil.

Works on a compilation line (`... [trailing-whitespace] ...'), on a Flymake
diagnostic (which carries the rule as data), and on a `C-h f'-style doc
buffer."
  (or (let ((checker (sift--diagnostic-checker (sift--diagnostic-at-point))))
        (and checker (symbol-name checker)))
    (save-excursion
      (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
        (when (string-match "\\[\\([a-z][a-z0-9-]+\\)\\]" line)
          (match-string 1 line))))))

(defun sift-explain-at-point ()
  "Explain the rule reported at point.

On a compilation line this reads the rule out of the line; on a line with a
Flymake diagnostic it reads the rule out of the diagnostic."
  (interactive)
  (let ((rule (sift--rule-at-point)))
    (if rule
      (sift-explain rule)
      (call-interactively #'sift-explain))))

;;; Flymake

(defun sift--position (line column)
  "Return the buffer position of 1-based LINE and COLUMN.

sift counts columns in characters, which is what `forward-char' moves by,
so a line of UTF-8 text lands where the executable said it would."
  (save-excursion
    (goto-char (point-min))
    (forward-line (max 0 (1- line)))
    (let ((target (+ (line-beginning-position) (max 0 (1- column)))))
      (min (point-max) target))))

(defun sift-diagnostic-code (diagnostic)
  "Return the sift rule code carried by a Flymake DIAGNOSTIC."
  (plist-get (flymake-diagnostic-data diagnostic) :code))

(defun sift--flymake-type (severity)
  "Return the Flymake diagnostic type for a sift SEVERITY."
  (cond ((equal severity "error") :error)
        ((equal severity "warning") :warning)
        (t :note)))

(defun sift--diagnostics (json-text)
  "Turn sift's JSON output into Flymake diagnostics."
  (let ((items (json-parse-string json-text
                                  :object-type 'alist
                                  :array-type 'list
                                  :null-object nil
                                  :false-object nil))
        (diagnostics '()))
    (dolist (item items (nreverse diagnostics))
      (let* ((line (alist-get 'line item))
             (column (alist-get 'column item))
             (checker (alist-get 'checker item))
             (code (alist-get 'code item))
             (severity (alist-get 'severity item))
             (message (alist-get 'message item))
             (begin (sift--position line column))
             (end (sift--position line (1+ column))))
        (push (flymake-make-diagnostic
                 (current-buffer) begin end
                 (sift--flymake-type severity)
                 ;; The rule's code leads the text: `flymake-show-buffer-diagnostics'
                 ;; then lists sift's codes, and `C-c s e' can find the rule by
                 ;; name.  A list here would be rendered as a list before Emacs 31,
                 ;; so the text is a string on every version.
                 (format "%s [%s] %s" code checker message)
                 ;; …and the rule name as data, which is what a code action
                 ;; needs to fix exactly this rule.
                 (list :checker (and checker (intern checker))
                       :code code
                       :severity severity))
          diagnostics)))))

(defvar-local sift--flymake-run nil
  "The sift run in flight for this buffer, as (GENERATION . PROCESS) or nil.

Flymake asks the backend again on every change, and two answers can be in
flight at once.  The generation says which question an answer belongs to, so an
answer to a question nobody is asking any more is dropped instead of being
reported about text that has already moved on -- which Flymake calls an
\"obsolete report\", and says so, once per keystroke, in *Messages*.")

(defun sift-flymake (report-fn &rest _arguments)
  "Flymake backend running `sift lint' on the current buffer.

The buffer text goes to sift on standard input, so nothing has to be saved
first, and the answer comes back as JSON.  Flymake calls this again on every
change; the run is asynchronous so that typing is never held up by it, the run
it replaces is cancelled so that its answer cannot arrive late, and an answer
that arrives for an older question is dropped."
  (let* ((origin (current-buffer))
         (dialect (sift--dialect))
         (label (sift--label))
         (source (buffer-substring-no-properties (point-min) (point-max)))
         (previous (and (consp sift--flymake-run) (cdr sift--flymake-run)))
         (generation (1+ (if (consp sift--flymake-run)
                           (car sift--flymake-run)
                           0))))
    ;; the question this one replaces is withdrawn before the new one is asked
    (sift--cancel previous)
    (setq sift--flymake-run (cons generation nil))
    (if (not dialect)
      (funcall report-fn :panic
               (format "sift: no dialect for %s; set `sift-dialect'" major-mode))
      (setcdr sift--flymake-run
              (sift--run-async
               (append (list "lint" "--stdin" "--format=json")
                       (sift--severity-options)
                       (list
                        (concat "--dialect=" dialect)
                        (concat "--label=" label)))
               source
               (lambda (result)
                 ;; the answer is about a buffer and about a question; both
                 ;; have to still be the current ones before it means anything
                 (when (buffer-live-p origin)
                   (with-current-buffer origin
                     (when (and (consp sift--flymake-run)
                                (= generation (car sift--flymake-run)))
                       (let ((failure (sift--failure result)))
                         (cond
                           (failure
                            (funcall report-fn :panic failure))
                           (t
                            (condition-case error
                                (funcall report-fn
                                         (sift--diagnostics
                                          (plist-get result :stdout)))
                              (error
                               (funcall report-fn :panic
                                        (format "sift: could not read its JSON: %s"
                                                (error-message-string error)))))))))))))))))

;;;###autoload
(define-minor-mode sift-flymake-mode
  "Report sift's findings in this buffer with Flymake."
  :lighter " sift"
  :group 'sift
  (if sift-flymake-mode
    (progn
      (unless (sift--dialect)
        (setq sift-flymake-mode nil)
        (user-error "No sift dialect for `%s'; set `sift-dialect'" major-mode))
      (add-hook 'flymake-diagnostic-functions #'sift-flymake nil t)
      (flymake-mode 1))
    (remove-hook 'flymake-diagnostic-functions #'sift-flymake t)))

;;; Eglot

;; sift speaks the Language Server Protocol, so Eglot can talk to it.  That is
;; the integration that has code actions: Eglot asks the server for the actions
;; that apply at point, and the server answers with sift's own fixes, which is
;; the same code `sift fix' runs.  The Flymake backend above is what remains
;; when there is no Eglot -- a second backend in an Eglot buffer would report
;; every finding twice.

(defcustom sift-eglot-modes '(scheme-mode emacs-lisp-mode lisp-mode)
  "The major modes Eglot starts a sift language server for."
  :type '(repeat symbol)
  :group 'sift)

(defcustom sift-eglot-command nil
  "The command that starts sift's language server.

Nil means `sift-executable' followed by `lsp', which is what almost everyone
wants: one setting names the program, and the server is that program with a
subcommand.  Set it when the server is somewhere else -- another build, or a
wrapper that adds arguments."
  :type '(choice (const :tag "sift-executable with `lsp'" nil)
           (repeat string))
  :group 'sift)

(defun sift-eglot-server-command ()
  "Return the argv that starts the language server."
  (or sift-eglot-command (list sift-executable "lsp")))

(defun sift-eglot-contact (&rest _)
  "Return the command Eglot runs, or say that sift is missing.

A program named by an absolute path is checked as a file as well as by
`executable-find', because that is what a user who set `sift-executable' to
one meant."
  (let ((command (sift-eglot-server-command)))
    (if (or (executable-find (car command))
          (file-executable-p (car command)))
      command
      (user-error "sift: `%s' is not executable; set `sift-executable' or `sift-eglot-command'"
        (car command)))))

(defun sift--eglot-silence-flymake ()
  "Keep the parts of sift that an Eglot server already does out of the buffer.

The server reports the findings Eglot shows through Flymake, so a buffer with
both backends carries every finding twice; and the server formats on save when
Eglot is asked to, so `sift-format-on-save' would format the same buffer a
second time and save it twice.  Turning both off here rather than telling the
user not to run `sift-flymake-setup' means both entry points can be safe."
  (when (bound-and-true-p eglot--managed-mode)
    (when (memq #'sift-flymake flymake-diagnostic-functions)
      (remove-hook 'flymake-diagnostic-functions #'sift-flymake t))
    (when (memq #'sift--format-before-save before-save-hook)
      (remove-hook 'before-save-hook #'sift--format-before-save t))))

(defun sift-code-actions ()
  "Offer the code actions sift's server has at point, through Eglot.

The actions are sift's own fixes -- the ones `sift fix' applies -- so the
buffer gets the same edit whether it was fixed here or by the executable."
  (interactive)
  (unless (fboundp 'eglot-code-actions)
    (user-error "sift: code actions need Eglot; run `sift-eglot-setup' first"))
  (unless (eglot-current-server)
    (user-error "sift: no language server here; start Eglot with `M-x eglot'"))
  (call-interactively #'eglot-code-actions))

(defun sift-eglot-setup ()
  "Give Eglot `sift lsp' for the Lisp modes, and turn `sift-mode' on.

Put it in your init file beside the other setup calls:

  (sift-eglot-setup)

The keymap and the format-on-save hook then come from `sift-mode', the
findings and the code actions from the server, and `sift-flymake-mode' stays
out of the way."
  (interactive)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
      (cons sift-eglot-modes #'sift-eglot-contact))
    (add-hook 'eglot-managed-mode-hook #'sift--eglot-silence-flymake))
  (dolist (mode sift-eglot-modes)
    (sift--add-mode-hook mode #'sift-mode)
    (sift--add-mode-hook mode #'sift--eglot-silence-flymake))
  t)

;;; The minor mode

(defvar sift-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s f") #'sift-format-buffer)
    (define-key map (kbd "C-c s r") #'sift-format-region)
    (define-key map (kbd "C-c s c") #'sift-check-buffer)
    (define-key map (kbd "C-c s l") #'sift-lint)
    (define-key map (kbd "C-c s x") #'sift-fix-buffer)
    (define-key map (kbd "C-c s a") #'sift-fix-at-point)
    (define-key map (kbd "C-c s e") #'sift-explain-at-point)
    (define-key map (kbd "C-c s p") #'sift-ci)
    (define-key map (kbd "C-c s d") #'sift-flymake-mode)
    (define-key map (kbd "C-c s A") #'sift-code-actions)
    map)
  "Keymap for `sift-mode'.")

(defvar sift-mode)

(defun sift--format-before-save ()
  "Format the buffer before saving it, when `sift-format-on-save' says so.

In a buffer an Eglot server is managing, the server formats on save -- that is
what `textDocument/formatting' is for, and what `eglot-format-mode' asks it to
do -- so formatting here as well would send the buffer out twice and save it
twice.  The server is the one that reads the project's own settings for the
edits it makes, so it is the one that gets to make them."
  (when (and sift-format-on-save sift-mode
          (not (bound-and-true-p eglot--managed-mode)))
    (sift-format-buffer)))

;;;###autoload
(define-minor-mode sift-mode
  "Format and lint the current buffer with sift.

The mode is a keymap and two hooks: `C-c s f' formats, `C-c s l' lints in a
compilation buffer, `C-c s x' applies every fix, `C-c s a' applies the fixes
of the rule at point, and `C-c s e' explains that rule.  Findings are shown
by `sift-flymake-mode', which `sift-setup' turns on with this mode."
  :lighter " sift"
  :keymap sift-mode-map
  :group 'sift
  (if sift-mode
    (progn
      (add-hook 'before-save-hook #'sift--format-before-save nil t)
      (when (and (not sift-dialect) (not (sift--dialect)))
        (message "sift: no dialect for `%s'; set `sift-dialect'" major-mode)))
    (remove-hook 'before-save-hook #'sift--format-before-save t)))

;;;###autoload
(defun sift-setup ()
  "Turn `sift-mode' on in the Lisp modes this package knows.

Use it from your init file instead of three `add-hook' calls:

  (sift-setup)"
  (sift--add-mode-hook 'scheme-mode #'sift-mode)
  (sift--add-mode-hook 'emacs-lisp-mode #'sift-mode)
  (sift--add-mode-hook 'lisp-mode #'sift-mode))

;;;###autoload
(defun sift-flymake-setup ()
  "Turn `sift-flymake-mode' on in the Lisp modes this package knows."
  (sift--add-mode-hook 'scheme-mode #'sift-flymake-mode)
  (sift--add-mode-hook 'emacs-lisp-mode #'sift-flymake-mode)
  (sift--add-mode-hook 'lisp-mode #'sift-flymake-mode))

(provide 'sift)
;;; sift.el ends here
