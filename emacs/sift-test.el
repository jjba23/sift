;;; sift-test.el --- tests for the sift Emacs package  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; Copyright (C) 2025, 2026  Josep Bigorra

;;; Commentary:

;; ERT is the test framework Emacs ships, and the only one this file needs:
;; the package is a thin wrapper around an executable, so the tests run the
;; executable and compare what comes back, the way a user would.

;;; Code:

(require 'ert)
(require 'sift)

(defconst sift-test--root
  (file-name-directory
     (directory-file-name
        (file-name-directory (or load-file-name buffer-file-name))))
  "The checkout these tests belong to.")

(defun sift-test--setup ()
  "Point the package at this checkout's executable."
  (setq sift-executable (expand-file-name "bin/sift" sift-test--root)
    sift-dialect nil
    sift-extra-arguments nil))

(sift-test--setup)

(defmacro sift-test--with-buffer (mode text &rest body)
  "Insert TEXT in a temporary buffer in MODE and run BODY."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,text)
     (setq major-mode ,mode)
     (sift-mode 1)
     ,@body))

;;; ===== Running the executable =====

(ert-deftest sift-test-version ()
  "The executable this package talks to is there and answers."
  (let ((result (sift--run '("version"))))
    (should (= 0 (plist-get result :status)))
    (should (string-prefix-p "sift " (plist-get result :stdout)))))

(ert-deftest sift-test-dialect-from-major-mode ()
  "Each Lisp mode maps to the dialect sift knows."
  (with-temp-buffer
    (setq major-mode 'scheme-mode)
    (should (equal "scheme" (sift--dialect))))
  (with-temp-buffer
    (setq major-mode 'emacs-lisp-mode)
    (should (equal "elisp" (sift--dialect))))
  (with-temp-buffer
    (setq major-mode 'lisp-mode)
    (should (equal "cl" (sift--dialect))))
  (with-temp-buffer
    (setq major-mode 'fundamental-mode)
    (should (null (sift--dialect)))))

;;; ===== Formatting =====

(ert-deftest sift-test-format-buffer ()
  "Formatting a buffer rewrites it and keeps the point inside it."
  (sift-test--with-buffer 'scheme-mode "(define (f x)\n(+ x 1))\n"
    (sift-format-buffer)
    (should (equal "(define (f x)\n  (+ x 1))\n" (buffer-string)))
    (should (<= (point) (point-max)))))

(ert-deftest sift-test-format-buffer-is-idempotent ()
  "Formatting twice changes nothing the second time."
  (sift-test--with-buffer 'scheme-mode "(define (f x)   (g x))\n"
    (sift-format-buffer)
    (let ((once (buffer-string)))
      (sift-format-buffer)
      (should (equal once (buffer-string))))))

(ert-deftest sift-test-format-elisp ()
  "The dialect of the buffer is the one sift is told about."
  (sift-test--with-buffer 'emacs-lisp-mode "(defun f (x)\n(message \"hi\"))\n"
    (sift-format-buffer)
    (should (equal "(defun f (x)\n  (message \"hi\"))\n" (buffer-string)))))

(ert-deftest sift-test-format-region ()
  "A region is formatted, and the rest of the buffer is untouched."
  (sift-test--with-buffer 'scheme-mode "(a)\n(b   c)\n"
    (goto-char (point-min))
    (forward-line 1)
    (sift-format-region (point) (point-max))
    (should (equal "(a)\n(b c)\n" (buffer-string)))))

(ert-deftest sift-test-format-broken-source-says-why ()
  "A buffer sift cannot read is left alone, and the reason is reported.

The reason is a `user-error' rather than a message in the echo area: a
formatter that quietly does nothing teaches people to believe it worked."
  (sift-test--with-buffer 'scheme-mode "(define (f x)\n  (a b\n"
    (let ((before (buffer-string))
          (message-text nil))
      (cl-letf (((symbol-function 'message) (lambda (format &rest args)
                                              (setq message-text (apply #'format format args)))))
        (should-error (sift-format-buffer))
        (should (equal before (buffer-string)))))))

(ert-deftest sift-test-check-buffer ()
  "Checking does not change the buffer."
  (sift-test--with-buffer 'scheme-mode "(a  b)\n"
    (let ((before (buffer-string)))
      (cl-letf (((symbol-function 'message) #'ignore))
        (sift-check-buffer))
      (should (equal before (buffer-string))))))

;;; ===== Fixing =====

(ert-deftest sift-test-fix-buffer ()
  "Every fix the linter has is applied to the buffer."
  (sift-test--with-buffer 'scheme-mode "(if (ready?) (go))   \n"
    (sift-fix-buffer)
    (should (equal "(when (ready?) (go))\n" (buffer-string)))))

(ert-deftest sift-test-fix-string-filters-by-checker ()
  "A fix can be restricted to one rule, which is what a code action does."
  (with-temp-buffer
    (setq major-mode 'scheme-mode)
    (let ((all (sift--fix-string "(if (a) (b))   \n" nil))
          (only-whitespace (sift--fix-string "(if (a) (b))   \n"
                             '(trailing-whitespace))))
      (should (equal "(when (a) (b))\n" all))
      (should (equal "(if (a) (b))\n" only-whitespace)))))

;;; ===== Flymake =====

(ert-deftest sift-test-diagnostics-shape ()
  "sift's JSON becomes Flymake diagnostics carrying the rule code and name."
  (with-temp-buffer
    (insert "(if (a) (b))\n")
    (let ((diagnostics
             (sift--diagnostics
                "[{\"file\":\"x.scm\",\"line\":1,\"column\":2,\"checker\":\"if-without-else\",\"code\":\"SIFT012\",\"severity\":\"warning\",\"message\":\"`if` has no else branch\"}]")))
      (should (= 1 (length diagnostics)))
      (let ((diagnostic (car diagnostics)))
        (should (equal "SIFT012" (sift-diagnostic-code diagnostic)))
        (should (eq 'if-without-else
                  (plist-get (flymake-diagnostic-data diagnostic) :checker)))
        (should (eq :warning (flymake-diagnostic-type diagnostic)))
        ;; line 1, column 2 is the second character of the buffer
        (should (equal (+ (point-min) 1) (flymake-diagnostic-beg diagnostic)))))))

(ert-deftest sift-test-diagnostics-empty ()
  "A clean run is an empty array, and no diagnostics."
  (with-temp-buffer
    (insert "(a)\n")
    (should (null (sift--diagnostics "[]")))))

(ert-deftest sift-test-flymake-backend ()
  "The backend runs the executable and reports what it found."
  (sift-test--with-buffer 'scheme-mode "(if (a) (b))\n"
    (let ((reported 'done))
      (sift-flymake (lambda (diagnostics &rest _) (setq reported diagnostics)))
      (with-timeout (10 (ert-fail "sift did not answer"))
        (while (eq reported 'done)
          (accept-process-output nil 0.1)))
      (should (listp reported))
      (should (not (eq :panic (car reported))))
      (should (= 1 (length reported)))
      (should (equal "SIFT012" (sift-diagnostic-code (car reported)))))))

(ert-deftest sift-test-flymake-backend-reports-a-parse-error ()
  "A buffer that does not parse has diagnostics, not silence.

A half-written form is the normal state of a buffer being typed, and a backend
that answers with nothing while the file is broken teaches people to ignore
it."
  (sift-test--with-buffer 'scheme-mode "(define (f x)\n  (if (ready?)\n"
    (let ((reported 'done))
      (sift-flymake (lambda (diagnostics &rest _) (setq reported diagnostics)))
      (with-timeout (10 (ert-fail "sift did not answer"))
        (while (eq reported 'done)
          (accept-process-output nil 0.1)))
      (should (not (eq :panic (car reported))))
      (should (> (length reported) 0))
      (should (equal :error (flymake-diagnostic-type (car reported))))
      (should (stringp (sift-diagnostic-code (car reported)))))))

(ert-deftest sift-test-format-buffer-refuses-a-parse-error ()
  "A buffer that does not parse is left alone, and says why."
  (sift-test--with-buffer 'scheme-mode "(define (f x)\n  (if (ready?)\n"
    (let ((before (buffer-string)))
      (should-error (sift-format-buffer))
      (should (equal before (buffer-string))))))

(ert-deftest sift-test-fix-buffer-refuses-a-parse-error ()
  "Fixing a buffer that does not parse is refused rather than guessed at."
  (sift-test--with-buffer 'scheme-mode "(define (f x)\n  (if (ready?)\n"
    (let ((before (buffer-string)))
      (should-error (sift-fix-buffer))
      (should (equal before (buffer-string))))))

(ert-deftest sift-test-flymake-backend-unknown-mode ()
  "In a mode with no dialect the backend disables itself with a reason."
  (with-temp-buffer
    (insert "(a)\n")
    (setq major-mode 'fundamental-mode)
    (let ((reported nil))
      ;; a panic is reported as (:panic EXPLANATION), not as a list of
      ;; diagnostics, so the arguments are captured as they come
      (sift-flymake (lambda (&rest arguments) (setq reported arguments)))
      (should (eq :panic (car reported)))
      (should (stringp (cadr reported))))))

;;; ===== Explaining rules =====

(ert-deftest sift-test-rule-at-point-from-compilation-line ()
  "The rule is read out of a lint line."
  (with-temp-buffer
    (insert "/tmp/x.scm:1:2: warning: [if-without-else] `if` has no else branch\n")
    (goto-char (point-min))
    (should (equal "if-without-else" (sift--rule-at-point)))))

(ert-deftest sift-test-explain ()
  "Explaining a rule shows its documentation and its code."
  (let ((buffer (get-buffer-create "*sift-explain*")))
    (unwind-protect
      (progn
        (sift-explain "if-without-else")
        (with-current-buffer buffer
          (should (string-match-p "SIFT012" (buffer-string)))
          (should (string-match-p "DIALECTS" (buffer-string)))))
      (kill-buffer buffer))))

(ert-deftest sift-test-ci-command ()
  "`sift-ci' runs the same check a pipeline runs, from the repository root.

The command is captured rather than run: the test is about the command and
the directory, not about how long linting this repository takes."
  (let ((captured nil) (directory nil))
    (cl-letf (((symbol-function 'compilation-start)
               (lambda (command &rest _)
                 (setq captured command)
                 (setq directory default-directory)
                 nil)))
      (sift-ci))
    (should (string-match-p "sift" captured))
    (should (string-match-p " ci" captured))
    (should (file-directory-p directory))))

(ert-deftest sift-test-rule-names ()
  "The rule names come from list-checkers, not from a list in this file."
  (let ((names (sift--rule-names)))
    (should (member "if-without-else" names))
    (should (member "clause-brackets" names))
    (should (> (length names) 30))))

;;; ===== Compilation mode =====

(ert-deftest sift-test-error-regexp ()
  "A lint line parses into a file, a line, a column and a rule."
  (with-temp-buffer
    (insert "/tmp/example.scm:12:5: warning: [empty-body] `when` form has no body\n")
    (goto-char (point-min))
    (should (re-search-forward sift--error-regexp nil t))
    (should (equal "/tmp/example.scm" (match-string 1)))
    (should (equal "12" (match-string 2)))
    (should (equal "5" (match-string 3)))
    (should (equal "warning" (match-string 4)))
    (should (equal "empty-body" (match-string 5)))))

;;; ===== Eglot =====

(ert-deftest sift-test-eglot-contact-runs-the-server ()
  "The contact Eglot is given is the language server, not the linter."
  (should (equal (list sift-executable "lsp") (sift-eglot-contact)))
  (let ((sift-eglot-command '("sift" "lsp" "--severity=warning")))
    (should (equal '("sift" "lsp" "--severity=warning")
              (sift-eglot-server-command)))))

(ert-deftest sift-test-eglot-contact-says-when-sift-is-missing ()
  "A program that is not there is a message, not a broken process."
  (let ((sift-executable "/nonexistent/sift"))
    (should-error (sift-eglot-contact) :type 'user-error)))

(ert-deftest sift-test-eglot-setup-registers-the-modes ()
  "After setup, Eglot finds sift for the three Lisp modes."
  (require 'eglot)
  (let ((eglot-server-programs eglot-server-programs))
    (sift-eglot-setup)
    (dolist (mode sift-eglot-modes)
      (should (eglot--lookup-mode mode)))))

(ert-deftest sift-test-eglot-setup-leaves-the-flymake-backend-alone ()
  "Setup does not turn the Flymake backend on: the server reports the findings.

`add-hook' writes a hook's default value, and `scheme-mode-hook' does not exist
until Scheme mode is loaded, so the question is asked about a hook that is
always there, and the answer restored afterwards."
  (require 'eglot)
  (let ((saved (default-value 'emacs-lisp-mode-hook)))
    (unwind-protect
      (progn
        (set-default 'emacs-lisp-mode-hook nil)
        (sift-eglot-setup)
        (should-not (memq #'sift-flymake-mode
                      (default-value 'emacs-lisp-mode-hook)))
        (should (memq #'sift-mode (default-value 'emacs-lisp-mode-hook)))
        (should (memq #'sift--eglot-silence-flymake
                  (default-value 'emacs-lisp-mode-hook))))
      (set-default 'emacs-lisp-mode-hook saved))))

(ert-deftest sift-test-eglot-silences-the-flymake-backend ()
  "In an Eglot-managed buffer, sift's own backend is removed."
  (with-temp-buffer
    (add-hook 'flymake-diagnostic-functions #'sift-flymake nil t)
    (let ((eglot--managed-mode t))
      (sift--eglot-silence-flymake))
    (should-not (memq #'sift-flymake flymake-diagnostic-functions))))

(ert-deftest sift-test-code-actions-need-a-server ()
  "Asking for code actions without Eglot says what to do instead of failing."
  (with-temp-buffer
    (should-error (sift-code-actions) :type 'user-error)))

(provide 'sift-test)
;;; sift-test.el ends here
