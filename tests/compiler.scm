;; sift --- what the real compilers say
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The reader knows the shape of a form; a compiler knows the names inside it.
;; These tests state both halves of the contract: the parser reads the lines
;; each implementation prints, and the runner asks the implementation that is
;; installed for an answer about a file it did not have to write into the tree.
;;
;; The parser tests use the exact text the three implementations printed on
;; this machine, so a format that changes is a failing test rather than a
;; silently smaller answer.  The runner tests skip a dialect whose compiler is
;; not installed, because a machine without SBCL is a machine sift still works
;; on.

(define-module (tests compiler)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (veritas veritas)
  #:use-module (sift compiler)
  #:use-module (tests helpers)
  #:export (spec))

(define guile-output
  "wrote `/tmp/bad.go'
/tmp/bad.scm:2:3: warning: possibly unbound variable `unbound-fn'
")

(define emacs-output
  "In toplevel form:
bad.el:1:1: Warning: file has no ‘lexical-binding’ directive on its first line

In end of data:
bad.el:2:4: Warning: the function ‘unbound-fn’ is not known to be defined.
")

(define sbcl-output
  "; compiling file \"/tmp/bad.lisp\" (written 11 NOV 2026 09:22:09 PM):

; file: /tmp/bad.lisp
; in: DEFUN F
;     (UNBOUND-FN X 1)
; 
; caught STYLE-WARNING:
;   undefined function: COMMON-LISP-USER::UNBOUND-FN
; 
; compilation unit finished
;   Undefined function:
;     UNBOUND-FN
;   caught 1 STYLE-WARNING condition

; wrote /tmp/bad.fasl
; compilation finished in 0:00:00.002
")

(define sbcl-error-output
  "; compiling file \"/tmp/bad2.lisp\":

; caught ERROR:
;   READ error during COMPILE-FILE:
;
;     end of file on #<SB-INT:FORM-TRACKING-STREAM for \"file /tmp/bad2.lisp\" {120003C163}>;
;
;     (in form starting at line: 1, column: 0, position: 0)
")

(define (message-of diagnostic)
  (compiler-diagnostic-message diagnostic))

(define (messages diagnostics)
  (map message-of diagnostics))

(define (spec)
  (suite "compiler: the implementation's own answer"

    ;; ===== the parsers =====

    (test "a Guile warning names its line and column"
      (let ((diagnostics (parse-guile-diagnostics guile-output)))
        (assert-equal #:expect 1 #:got (length diagnostics))
        (let ((diagnostic (car diagnostics)))
          (assert-equal #:expect 'guild #:got (compiler-diagnostic-tool diagnostic))
          (assert-equal #:expect 'warning
            #:got (compiler-diagnostic-severity diagnostic))
          (assert-equal #:expect 1 #:got (compiler-diagnostic-line diagnostic))
          (assert-equal #:expect 2 #:got (compiler-diagnostic-column diagnostic))
          (assert-contains #:expect (message-of diagnostic)
            #:got "unbound-fn"))))

    (test "an Emacs warning is read with and without a column"
      (let ((diagnostics (parse-emacs-diagnostics emacs-output)))
        (assert-equal #:expect 2 #:got (length diagnostics))
        (assert-equal #:expect 0
          #:got (compiler-diagnostic-line (car diagnostics)))
        (assert-equal #:expect 0
          #:got (compiler-diagnostic-column (car diagnostics)))
        (assert-equal #:expect 1
          #:got (compiler-diagnostic-line (cadr diagnostics)))
        (assert-equal #:expect 3
          #:got (compiler-diagnostic-column (cadr diagnostics)))))

    (test "an SBCL style warning is a warning, located by the form it echoed"
      (let* ((text "(defun f (x)\n  (unbound-fn x 1))\n")
             (diagnostics (parse-sbcl-diagnostics sbcl-output text)))
        (assert-equal #:expect 1 #:got (length diagnostics))
        (let ((diagnostic (car diagnostics)))
          (assert-equal #:expect 'sbcl #:got (compiler-diagnostic-tool diagnostic))
          (assert-equal #:expect 'warning
            #:got (compiler-diagnostic-severity diagnostic))
          (assert-equal #:expect 1 #:got (compiler-diagnostic-line diagnostic))
          (assert-equal #:expect 2 #:got (compiler-diagnostic-column diagnostic))
          (assert-contains #:expect (message-of diagnostic)
            #:got "undefined function"))))

    (test "an SBCL read error is an error at the form that started it"
      (let ((diagnostics (parse-sbcl-diagnostics sbcl-error-output
                                                 "(defun f (x)\n  (+ x 1)\n")))
        (assert-equal #:expect 1 #:got (length diagnostics))
        (assert-equal #:expect 'error
          #:got (compiler-diagnostic-severity (car diagnostics)))
        (assert-equal #:expect 0
          #:got (compiler-diagnostic-line (car diagnostics)))
        (assert-equal #:expect 0
          #:got (compiler-diagnostic-column (car diagnostics)))))

    (test "a compiler that says nothing is no diagnostics"
      (assert-equal #:expect '() #:got (parse-guile-diagnostics ""))
      (assert-equal #:expect '() #:got (parse-emacs-diagnostics "")))

    ;; ===== the runner =====

    (test "a Scheme file is compiled and its warning is returned"
      (if (not (compiler-available? 'scheme))
        (assert-true #t #:name "guild is not installed; skipped")
        (let ((diagnostics (compile-text
                            "(define (f x)\n  (unbound-fn x 1))\n"
                            #:dialect 'scheme)))
          (assert-true (and (pair? diagnostics) #t)
            #:name "the compiler had something to say")
          (assert-true (and (any (lambda (diagnostic)
                                   (string-contains (string-downcase
                                                     (message-of diagnostic))
                                                    "unbound-fn"))
                                 diagnostics)
                         #t)
            #:name "about the name that was never defined"))))

    (test "an Emacs Lisp file is compiled and its warning is returned"
      (if (not (compiler-available? 'elisp))
        (assert-true #t #:name "emacs is not installed; skipped")
        (let ((diagnostics (compile-text
                            "(defun f (x)\n  (unbound-fn x 1))\n"
                            #:dialect 'elisp)))
          (assert-true (and (any (lambda (diagnostic)
                                   (string-contains (string-downcase
                                                     (message-of diagnostic))
                                                    "unbound-fn"))
                                 diagnostics)
                         #t)
            #:name "about the function that was never defined"))))

    (test "a Common Lisp file is compiled when SBCL is installed"
      (if (not (compiler-available? 'cl))
        (assert-true #t #:name "sbcl is not installed; skipped")
        (let ((diagnostics (compile-text
                            "(defun f (x)\n  (unbound-fn x 1))\n"
                            #:dialect 'cl)))
          (assert-true (and (any (lambda (diagnostic)
                                   (string-contains (string-downcase
                                                     (message-of diagnostic))
                                                    "unbound-fn"))
                                 diagnostics)
                         #t)
            #:name "about the function that was never defined"))))

    (test "a compile writes nothing into the project"
      (if (not (compiler-available? 'scheme))
        (assert-true #t #:name "guild is not installed; skipped")
        (with-temp-directory
         (lambda (directory)
           (let ((source (string-append directory "/probe.scm")))
             (spit source "(define (f x) (unbound-fn x))\n")
             (compile-text "(define (f x) (unbound-fn x))\n"
                           #:dialect 'scheme #:file source
                           #:root directory)
             (assert-equal #:expect '("probe.scm")
               #:got (sort (scandir directory
                                    (lambda (name)
                                      (not (member name '("." "..")))))
                           string<?)))))))))
