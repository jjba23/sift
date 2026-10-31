;;; sift.el --- sift formatting, linting and Flymake integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Josep Bigorra

;; Author: Josep Bigorra <jjbigorra@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: languages, lisp, tools
;; URL: https://codeberg.org/jjba23/sift

;; This file is not part of GNU Emacs.

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

(defgroup sift nil
  "Formatting and linting for Scheme, Emacs Lisp and Common Lisp."
  :group 'languages
  :prefix "sift-"
  :link '(url-link "https://codeberg.org/jjba23/sift"))

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
  "Minimum severity the linter reports."
  :type '(choice (const "error") (const "warning") (const "info"))
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

CALLBACK receives the same plist `sift--run' returns.  Flymake is given the
buffer back as soon as sift answers, which is what keeps typing responsive
on a large file."
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
             (when (memq (process-status proc) '(exit signal))
               (let ((status (process-exit-status proc))
                     (stdout (with-current-buffer stdout-buffer (buffer-string)))
                     (stderr (with-current-buffer stderr-buffer (buffer-string))))
                 (kill-buffer stdout-buffer)
                 (kill-buffer stderr-buffer)
                 (funcall callback
                          (list :status status :stdout stdout :stderr stderr))))))))
    (process-send-string process source)
    (process-send-eof process)
    process))

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
  "Return TEXT formatted with sift, or nil if it could not be."
  (let* ((dialect (sift--require-dialect))
         (result (sift--run
                  (append (list "format" "--stdin" "--stdout"
                                (concat "--label=" (sift--label))
                                (concat "--dialect=" dialect)))
                  text)))
    (if (sift--failure result)
        (progn
          (message "sift: %s" (sift--failure result))
          nil)
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
     ((null formatted) nil)
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
     ((null formatted) nil)
     ((string= formatted text) (message "sift: already formatted"))
     (t
      (atomic-change-group
        (delete-region beg end)
        (goto-char beg)
        (insert formatted))
      (message "sift: formatted the region")))))

;;; Fixing

(defun sift--fix-string (text &optional checkers)
  "Return TEXT with sift's fixes applied, or nil.

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
        (progn
          (message "sift: %s" (sift--failure result))
          nil)
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
     ((null fixed) nil)
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
  "Ask sift whether the buffer is formatted, and say so."
  (interactive)
  (let* ((source (buffer-substring-no-properties (point-min) (point-max)))
         (result (sift--run
                  (append (list "check" "--stdin"
                                (concat "--label=" (sift--label))
                                (concat "--dialect=" (sift--require-dialect)))
                          nil)
                  source)))
    (if (sift--failure result)
        (message "sift: %s" (sift--failure result))
      (if (= 0 (plist-get result :status))
          (message "sift: formatted")
        (message "sift: this buffer would be reformatted (M-x sift-format-buffer)")))))

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
                                 "lint"
                                 (concat "--severity=" sift-lint-severity))
                           (sift--options)
                           (when dialect (list (concat "--dialect=" dialect)))
                           (list (shell-quote-argument (buffer-file-name))))
                   " ")))
    (compilation-start command nil (lambda (_mode) "*sift-lint*"))))

;;; Explaining rules

(defun sift-explain (rule)
  "Show sift's documentation of RULE in a buffer."
  (interactive
   (list (completing-read "Rule: "
                          (sift--rule-names) nil nil
                          (sift--rule-at-point))))
  (let ((result (sift--run (list "explain" rule)))
        (buffer (get-buffer-create "*sift-explain*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (if (= 0 (plist-get result :status))
                    (plist-get result :stdout)
                  (string-trim (plist-get result :stderr))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buffer)))

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

(defun sift--rule-names ()
  "Return every rule name sift knows about."
  (let ((result (sift--run (list "list-checkers"))))
    (when (= 0 (plist-get result :status))
      (delq nil
            (mapcar (lambda (line)
                      (when (string-match "^\\([a-z0-9-]+\\) +SIFT[0-9]+" line)
                        (match-string 1 line)))
                    (split-string (plist-get result :stdout) "\n"))))))

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

(defun sift-flymake (report-fn &rest _arguments)
  "Flymake backend running `sift lint' on the current buffer.

The buffer text goes to sift on standard input, so nothing has to be saved
first, and the answer comes back as JSON.  Flymake calls this again on
every change; the run is asynchronous so that typing is never held up by
it."
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (dialect (sift--dialect))
        (label (sift--label))
        ;; the answer arrives in whichever buffer is current when the process
        ;; exits, so the buffer the question was asked about is captured now
        (origin (current-buffer)))
    (if (not dialect)
        (funcall report-fn :panic
                 (format "sift: no dialect for %s; set `sift-dialect'" major-mode))
      (sift--run-async
       (list "lint" "--stdin" "--format=json"
             (concat "--severity=" sift-lint-severity)
             (concat "--dialect=" dialect)
             (concat "--label=" label))
       source
       (lambda (result)
         (let ((failure (sift--failure result)))
           (cond
            (failure
             (funcall report-fn :panic failure))
            ;; a buffer that was killed while sift was running has nothing
            ;; left to report into
            ((not (buffer-live-p origin)) nil)
            (t
             (with-current-buffer origin
               (condition-case error
                   (funcall report-fn (sift--diagnostics (plist-get result :stdout)))
                 (error
                  (funcall report-fn :panic
                           (format "sift: could not read its JSON: %s"
                                   (error-message-string error))))))))))))))

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
    map)
  "Keymap for `sift-mode'.")

(defvar sift-mode)

(defun sift--format-before-save ()
  "Format the buffer before saving it, when `sift-format-on-save' says so."
  (when (and sift-format-on-save sift-mode)
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
  (add-hook 'scheme-mode-hook #'sift-mode)
  (add-hook 'emacs-lisp-mode-hook #'sift-mode)
  (add-hook 'lisp-mode-hook #'sift-mode))

;;;###autoload
(defun sift-flymake-setup ()
  "Turn `sift-flymake-mode' on in the Lisp modes this package knows."
  (add-hook 'scheme-mode-hook #'sift-flymake-mode)
  (add-hook 'emacs-lisp-mode-hook #'sift-flymake-mode)
  (add-hook 'lisp-mode-hook #'sift-flymake-mode))

(provide 'sift)
;;; sift.el ends here
