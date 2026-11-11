;; sift --- what the real compilers say
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The reader in `(sift reader)' is a reader, not a compiler: it knows the
;; shape of a form and nothing about the names inside it.  The implementation
;; a project already uses knows a great deal more -- an undefined variable, a
;; call with the wrong number of arguments, a macro that is not known, a type
;; that cannot hold the value -- and it says so in a form a human has read for
;; decades.  This module asks it, once per file, and turns the answer into
;; something a language server can publish.
;;
;; The compilers are run on a *temporary* copy of the text, never on the file
;; in the project: a server that compiled `foo.go' or `foo.elc' beside the
;; source would be a server that writes into the tree it is judging.  Each
;; implementation is asked in its own words, its output on standard error is
;; captured with standard output, and a small parser per implementation turns
;; the lines it prints into diagnostics.
;;
;; Nothing here is on the request path.  A compile is a process, a process is
;; slower than a keystroke, and the answer is published as a later report for
;; the version it was computed from.  A compiler that is not installed is not
;; an error: the answer is an empty list, and the rest of the server answers
;; exactly as it did before.
;;
;; The limits are the ones a background job needs: the output is bounded, the
;; temporary files are removed, and a compiler that hangs is left to the
;; process table rather than to the editor.  `shortcut: no hard timeout, a
;; compiler that hangs holds a thread until the session ends; add a watchdog
;; when a real implementation is found hanging.'

(define-module (sift compiler)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:export (compiler-diagnostic?
            compiler-diagnostic-tool
            compiler-diagnostic-severity
            compiler-diagnostic-message
            compiler-diagnostic-line
            compiler-diagnostic-column
            compiler-diagnostic-end-line
            compiler-diagnostic-end-column
            parse-guile-diagnostics
            parse-emacs-diagnostics
            parse-sbcl-diagnostics
            compiler-available?
            compile-text))

;;; ===== One finding a compiler made =====
;;;
;;; The position is the protocol's: a zero-based line and character.  A
;;; compiler that named only a line gets a one-character span on it, which is
;;; what an editor can underline; a compiler that named no position at all
;;; gets the first character of the file, and its message is the whole of the
;;; information it gave.

(define-record-type <compiler-diagnostic>
  (make-compiler-diagnostic tool severity message line column end-line end-column)
  compiler-diagnostic?
  (tool compiler-diagnostic-tool)
  (severity compiler-diagnostic-severity)
  (message compiler-diagnostic-message)
  (line compiler-diagnostic-line)
  (column compiler-diagnostic-column)
  (end-line compiler-diagnostic-end-line)
  (end-column compiler-diagnostic-end-column))

(define (diagnostic-at tool severity message line column)
  "Return a diagnostic at @var{line} and @var{column}, underlined by one."
  (make-compiler-diagnostic tool severity message line column line (+ column 1)))

(define (severity-of-word word)
  "Return the severity a compiler's word names."
  (cond
    [(string-ci=? word "error") 'error]
    [(string-ci=? word "warning") 'warning]
    [(string-ci=? word "note") 'info]
    [else 'warning]))

;;; ===== Guile =====
;;;
;;; `guild compile' prints `file:line:column: warning: message' to standard
;;; error, and the file is the temporary one this module wrote.  The path is
;;; deliberately matched greedily from the left: a directory may contain a
;;; colon, and the two numbers before the word are the part that is certain.

(define guile-line
  (make-regexp "^(.*):([0-9]+):([0-9]+): (warning|error|note):[ \t]*(.*)$"
               regexp/icase))

(define (guile-diagnostic line)
  "Return the diagnostic @var{line} spells, or @code{#f}."
  (let ((match (regexp-exec guile-line line)))
    (and match
      (diagnostic-at 'guild
                     (severity-of-word (match:substring match 4))
                     (string-trim-both (match:substring match 5))
                     (- (string->number (match:substring match 2)) 1)
                     (- (string->number (match:substring match 3)) 1)))))

(define (parse-guile-diagnostics output)
  "Return the diagnostics @var{output} holds, in the order it printed them."
  (filter-map guile-diagnostic (string-split output #\newline)))

;;; ===== Emacs Lisp =====
;;;
;;; The byte compiler prints `file:line:column: Warning: message', and some
;;; messages carry a line but no column.  Both shapes are read; the rest of
;;; its output is context a human reads and a server has no use for.

(define emacs-located-line
  (make-regexp "^(.*):([0-9]+):([0-9]+): (warning|error|note)( \\([^)]*\\))?:[ \t]*(.*)$"
               regexp/icase))
(define emacs-line
  (make-regexp "^(.*):([0-9]+): (warning|error|note)( \\([^)]*\\))?:[ \t]*(.*)$"
               regexp/icase))

(define (emacs-diagnostic line)
  "Return the diagnostic @var{line} spells, or @code{#f}."
  (let ((located (regexp-exec emacs-located-line line))
        (bare (regexp-exec emacs-line line)))
    (cond
      [located
        (diagnostic-at 'emacs
                       (severity-of-word (match:substring located 4))
                       (string-trim-both (match:substring located 6))
                       (- (string->number (match:substring located 2)) 1)
                       (- (string->number (match:substring located 3)) 1))]
      [bare
        (diagnostic-at 'emacs
                       (severity-of-word (match:substring bare 3))
                       (string-trim-both (match:substring bare 5))
                       (- (string->number (match:substring bare 2)) 1)
                       0)]
      [else #f])))

(define (parse-emacs-diagnostics output)
  "Return the diagnostics @var{output} holds, in the order it printed them."
  (filter-map emacs-diagnostic (string-split output #\newline)))

;;; ===== SBCL =====
;;;
;;; SBCL's compiler speaks in blocks:
;;;
;;;   ; in: DEFUN F
;;;   ;     (UNBOUND-FN X 1)
;;;   ;
;;;   ; caught STYLE-WARNING:
;;;   ;   undefined function: COMMON-LISP-USER::UNBOUND-FN
;;;
;;; The block names a condition rather than a position, so the position is
;;; recovered from the form SBCL echoed -- the source line is searched for,
;;; case-insensitively -- and from `(in form starting at line: N, column: M)',
;;; which a read error prints.  A condition that names neither lands on the
;;; first character, which is where a reader starts looking.

(define sbcl-caught
  (make-regexp "^; caught (STYLE-WARNING|WARNING|ERROR):"))
(define sbcl-form-position
  (make-regexp "in form starting at line: ([0-9]+), column: ([0-9]+)"))

(define (sbcl-severity word)
  (if (string-ci=? word "ERROR") 'error 'warning))

(define (position-of-snippet text snippet)
  "Return @code{(line . column)} where @var{snippet} first appears in @var{text}."
  (let ((at (string-contains (string-downcase text) (string-downcase snippet))))
    (if at
      (let* ((before (substring text 0 at))
             (line (- (length (string-split before #\newline)) 1))
             (slash (string-rindex before #\newline))
             (column (if slash (- (string-length before) slash 1) at)))
        (cons line column))
      (cons 0 0))))

(define (parse-sbcl-diagnostics output text)
  "Return the diagnostics @var{output} describes about @var{text}."
  (let loop ((lines (string-split output #\newline))
             (severity #f)
             (message '())
             (form #f)
             (snippet '())
             (found '()))
    (define (finish found)
      (if (not severity)
        found
        (let* ((line (if form (car form) 0))
               (column (if form (cdr form) 0))
               (position (if form
                           (cons line column)
                           (position-of-snippet text (string-join (reverse snippet) " ")))))
          (cons (make-compiler-diagnostic 'sbcl severity
                                          (string-join (reverse message) " ")
                                          (car position) (cdr position)
                                          (car position) (+ (cdr position) 1))
                found))))
    (cond
      [(null? lines) (reverse (finish found))]
      [else
        (let ((line (car lines)))
          (cond
            [(regexp-exec sbcl-caught line)
             (let* ((match (regexp-exec sbcl-caught line))
                    (found (finish found)))
               (loop (cdr lines)
                     (sbcl-severity (match:substring match 1))
                     '() #f snippet found))]
            [(and (not severity) (string-prefix? ";     " line))
             (loop (cdr lines) severity message form
                   (cons (string-trim (substring line 6)) snippet) found)]
            [(regexp-exec sbcl-form-position line)
             (let ((match (regexp-exec sbcl-form-position line)))
               (loop (cdr lines) severity message
                     (cons (- (string->number (match:substring match 1)) 1)
                           (string->number (match:substring match 2)))
                     snippet found))]
            [(string-prefix? ";   " line)
             (loop (cdr lines) severity
                   (cons (string-trim (substring line 4)) message)
                   form snippet found)]
            [(or (string-prefix? "; compilation" line)
               (string-prefix? "; wrote" line)
               (string-prefix? "; compiling" line))
             (loop (cdr lines) #f '() #f '() (finish found))]
            [else (loop (cdr lines) severity message form snippet found)]))])))

;;; ===== Asking one =====

(define dialect-program
  '((scheme . "guild")
    (elisp . "emacs")
    (cl . "sbcl")))

(define dialect-extension
  '((scheme . "scm")
    (elisp . "el")
    (cl . "lisp")))

(define* (compiler-available? dialect)
  "Return @code{#t} when @var{dialect}'s compiler is on @code{PATH}."
  (let ((program (assq-ref dialect-program dialect)))
    (and program
      (let ((path (getenv "PATH")))
        (and path
          (any (lambda (directory)
                 (file-exists? (string-append directory "/" program)))
               (string-split path #\:)))))))

(define *temporary-counter* 0)
(define *temporary-lock* (make-mutex))

(define (temporary-path dialect)
  "Return a fresh path in the temporary directory with @var{dialect}'s suffix."
  (with-mutex *temporary-lock*
              (set! *temporary-counter* (+ *temporary-counter* 1))
              (format #f "~a/sift-compile-~a-~a.~a"
                      (or (getenv "TMPDIR") "/tmp")
                      (getpid) *temporary-counter*
                      (assq-ref dialect-extension dialect))))

(define (quoted-string text)
  "Return @var{text} as a double-quoted string a Lisp reads.

Backslash and double quote are the two characters a path can hold that would
end the string early; everything else, UTF-8 included, is written as it is."
  (call-with-output-string
    (lambda (out)
      (display #\" out)
      (string-for-each
       (lambda (character)
         (when (or (char=? character #\\) (char=? character #\"))
           (display #\\ out))
         (display character out))
       text)
      (display #\" out))))

(define (elisp-script file output load-paths)
  "Return the Emacs Lisp program that compiles @var{file} into @var{output}."
  (format #f
          "(progn (setq byte-compile-dest-file-function (lambda (f) ~a))~@[~%~{~a~}~]~%(byte-compile-file ~a))"
          (quoted-string output)
          (map (lambda (directory)
                 (format #f "(add-to-list 'load-path ~a)" (quoted-string directory)))
               load-paths)
          (quoted-string file)))

(define (sbcl-script file output)
  "Return the Common Lisp program that compiles @var{file} into @var{output}."
  (format #f "(compile-file ~a :output-file ~a)"
          (quoted-string file) (quoted-string output)))

(define* (compiler-command dialect file output load-paths)
  "Return @code{(program . arguments)} that compiles @var{file}."
  (case dialect
    [(scheme)
     (cons "guild"
           (append (list "compile" "-o" output)
                   (append-map (lambda (directory) (list "-L" directory))
                               load-paths)
                   (list file)))]
    [(elisp)
     (cons "emacs"
           (list "--batch" "-Q" "--eval"
                 (elisp-script file output load-paths)))]
    [(cl)
     (cons "sbcl"
           (list "--noinform" "--non-interactive" "--disable-debugger"
                 "--eval" (sbcl-script file output)))]
    [else #f]))

(define output-limit 1000000)

(define* (run-program program arguments)
  "Run @var{program} with @var{arguments}, returning the output it printed.

The shell is the portable way to put a child's standard error where its
standard output goes: the script is a constant, and the program and its
arguments travel in @code{$@} as words of their own.  A program that cannot be
started, or that prints more than `output-limit' characters, is the output it
managed to print."
  (catch #t
         (lambda ()
           (let ((port (apply open-pipe* OPEN_READ
                              "sh" "-c" "exec \"$@\" 2>&1" "sh"
                              program arguments)))
             (let* ((text (get-string-n port output-limit))
                    ;; copied before the pipe is closed: a string from a port
                    ;; may share the port's buffer, and closing frees it
                    (copy (and (string? text) (string-copy text))))
               (close-pipe port)
               (or copy ""))))
         (lambda (key . rest) "")))

(define* (compile-text text #:key (dialect 'scheme) (file #f) (root #f))
  "Return the diagnostics @var{dialect}'s compiler makes about @var{text}.

@var{file} is the name the text has in the project, when it has one; its
directory and @var{root} are put on the compiler's load path so that a project
whose modules live under ~src/~ can be compiled from anywhere.  The text is
written to a temporary file and removed afterwards: nothing is written into
the project, and an unsaved buffer is compiled as it is.

A compiler that is not installed, or that fails before printing anything, is
an empty list of findings.  The caller already has a smaller answer for that
case."
  (let ((program (assq-ref dialect-program dialect))
        (extension (assq-ref dialect-extension dialect)))
    (if (not (and program extension text))
      '()
      (let* ((source (temporary-path dialect))
             (output (temporary-path dialect))
             (load-paths (delete-duplicates
                          (filter (lambda (path)
                                    (and path (not (string-null? path))
                                      (file-exists? path)))
                                  (append (if file (list (dirname file)) '())
                                          (if root (list root) '())
                                          (if root
                                            (list (string-append root "/src"))
                                            '())))
                          string=?)))
        (dynamic-wind
          (lambda ()
            (false-if-exception
             (call-with-output-file source (lambda (port) (display text port)))))
          (lambda ()
            (let ((command (compiler-command dialect source output load-paths)))
              (if (not command)
                '()
                (let ((printed (run-program (car command) (cdr command))))
                  (case dialect
                    [(scheme) (parse-guile-diagnostics printed)]
                    [(elisp) (parse-emacs-diagnostics printed)]
                    [(cl) (parse-sbcl-diagnostics printed text)]
                    [else '()])))))
          (lambda ()
            (false-if-exception (delete-file source))
            (false-if-exception (delete-file output))))))))
