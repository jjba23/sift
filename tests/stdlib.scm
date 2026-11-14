;; sift --- the standard library on disk
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A jump out of this file is only as good as what sift knows about the code
;; the file does not contain, and the largest such code base is the one every
;; dialect ships with itself.  These tests state what is read out of it.
;;
;; Two indexes are built from a standard library and both are tested here
;; against text and directories a test writes down: the *builtins*, a file of
;; one entry per name an implementation documents about itself, and the
;; *sources*, the files of the library read the way the project's own files
;; are.  Neither needs Guile, Emacs, SBCL or Racket to be installed to be
;; tested, which is the point of keeping the parsing away from the discovery:
;; the half that reads a format is the half that can be wrong, and it is
;; testable with a string.

(define-module (tests stdlib)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (sift records)
  #:use-module (sift definitions)
  #:use-module (sift stdlib)
  #:use-module (tests helpers)
  #:export (spec))

;;; ===== A library a test writes down =====

(define generated-source
  (string-append
   ";; sift --- the names GNU Guile 3.0.9 documents about itself\n"
   ";;\n"
   ";; Written by asking the implementation once.  A procedure is a `define'\n"
   ";; with a docstring, and a value is a `define' whose documentation is the\n"
   ";; comment above it.\n"
   ";;\n"
   ";; sift: version 3.0.9\n"
   ";; sift: root /usr/share/guile/3.0\n"
   "\n"
   "(define (car pair)\n"
   "  \"Return the contents of the car field of pair.\"\n"
   "  #f)\n"
   "\n"
   ";; The empty list.\n"
   "(define null '())\n"
   "\n"
   "(define-macro (when test . body)\n"
   "  \"Evaluate body when test is true.\"\n"
   "  #f)\n"))

(define* (library-of directory #:key (builtins #f))
  "Return a standard library whose sources are the files under DIRECTORY.

BUILTINS, when given, is the file an implementation described itself in -- the
second half of the library, written as source like the first."
  (index-standard-library #:dialect 'scheme
                          #:roots (list directory)
                          #:builtins-path builtins))

(define (names-of library)
  (map car (stdlib-names library)))

(define (root-ending roots suffix)
  "Return the root of @var{roots} that ends in @var{suffix}, or @code{#f}.

A temporary directory is a symlink on some systems, so a root is matched by the
path it ends in rather than by the string the test wrote down."
  (find (lambda (root) (string-suffix? suffix root)) roots))

;;; ===== The generated index =====

(define (spec)
  (suite "stdlib: the standard library on disk"
    ;; the tests set HOME, PATH, PLTCOLLECTS and XDG_CACHE_HOME for the
    ;; discovery they check, and an environment variable is process-wide: two
    ;; tests in flight would read each other's answer
    #:concurrent? #f

    (test "the metadata lines say which implementation wrote the file"
      (let ((metadata (metadata-of generated-source)))
        (assert-equal #:expect "3.0.9" #:got (cdr (assoc "version" metadata)))
        (assert-equal #:expect "/usr/share/guile/3.0"
          #:got (cdr (assoc "root" metadata)))))

    (test "the generated file is source: its definitions and docstrings are read"
      (with-temp-directory
       (lambda (directory)
         (spit (string-append directory "/scheme-builtins.scm") generated-source)
         (let* ((builtins (string-append directory "/scheme-builtins.scm"))
                (library (library-of directory #:builtins builtins))
                (car-site (car (stdlib-definitions library "car")))
                (definition (caddr car-site)))
           (assert-equal #:expect "car" #:got (definition-name definition))
           (assert-equal #:expect 'function #:got (definition-kind definition))
           ;; the reader keeps the quotes it read, and hover strips them
           (assert-equal #:expect "\"Return the contents of the car field of pair.\""
             #:got (definition-docstring definition))
           (assert-true (and (definition-form definition) #t)
             #:name "a builtin is a form a reader can read around")
           (assert-true (string-suffix? "/scheme-builtins.scm" (car car-site))
             #:name "and the answer names the generated file")))))

    (test "a macro and a value are read as the dialect writes them"
      (with-temp-directory
       (lambda (directory)
         (spit (string-append directory "/scheme-builtins.scm") generated-source)
         (let* ((library (library-of directory
                                     #:builtins (string-append
                                                 directory "/scheme-builtins.scm")))
                (macro (caddr (car (stdlib-definitions library "when"))))
                (value (caddr (car (stdlib-definitions library "null")))))
           (assert-equal #:expect 'macro #:got (definition-kind macro))
           (assert-equal #:expect "\"Evaluate body when test is true.\""
             #:got (definition-docstring macro))
           (assert-equal #:expect 'variable #:got (definition-kind value))
           (assert-equal #:expect #f #:got (definition-docstring value)
             #:name "Scheme has no docstring slot for a value; the comment
carries it")))))

    (test "a real source definition is offered before the generated file"
      ;; the generated file is the answer of last resort: a project, a
      ;; required library or the installation's own source wins
      (with-temp-directory
       (lambda (directory)
         (spit (string-append directory "/scheme-builtins.scm") generated-source)
         (spit (string-append directory "/car.scm")
               "(define (car pair)\n  (primitive-car pair))\n")
         (let* ((builtins (string-append directory "/scheme-builtins.scm"))
                (library (library-of directory #:builtins builtins))
                (sites (stdlib-definitions library "car")))
           (assert-equal #:expect 2 #:got (length sites))
           (assert-true (string-suffix? "/car.scm" (car (car sites)))
             #:name "the source file comes first")
           (assert-true (string-suffix? "/scheme-builtins.scm"
                                        (car (cadr sites)))
             #:name "and the generated file last")))))

    (test "the generated file is read with the limit it was written under"
      ;; it holds every name an implementation documents, so it is larger than
      ;; the 500 kB a source file is read up to
      (with-temp-directory
       (lambda (directory)
         (let ((builtins (string-append directory "/scheme-builtins.scm"))
               (doc (make-string 600000 #\x)))
           (spit builtins
                 (string-append "(define (big-name)\n  \"" doc "\"\n  #f)\n"))
           (let ((library (library-of directory #:builtins builtins)))
             (assert-equal #:expect 1
               #:got (length (stdlib-definitions library "big-name"))))))))

    (test "a root that another root contains is not walked twice"
      (assert-equal #:expect (list "/a" "/c" "/d/e")
        #:got (without-nested-roots
               (list "/a" "/a/b" "/c" "/c/d" "/d/e"))))

    (test "roots a standard library really has are all kept"
      (assert-equal #:expect (list "/usr/share/guile/site/3.0"
                                   "/usr/share/guile/3.0")
        #:got (without-nested-roots
               (list "/usr/share/guile/site/3.0" "/usr/share/guile/3.0"))))

    ;; ===== the sources =====

    (test "a definition in a library file is found by name"
      (with-temp-directory
       (lambda (directory)
         (spit (string-append directory "/base.scm")
               "(define (make-widget size)\n  (* size 2))\n")
         (let* ((library (library-of directory))
                (sites (stdlib-definitions library "make-widget"))
                (definition (and (pair? sites) (caddr (car sites)))))
           (assert-equal #:expect 1 #:got (length sites))
           (assert-equal #:expect "make-widget" #:got (definition-name definition))
           (assert-equal #:expect 'function #:got (definition-kind definition))
           (assert-true (string-suffix? "/base.scm" (car (car sites))))))))

    (test "a compressed library source is read like the file it was"
      ;; the Emacs library is installed compressed, and reading only the
      ;; uncompressed half leaves every name it defines out of the index
      (with-temp-directory
       (lambda (directory)
         (copy-file (fixture "compressed" "widget.el.gz")
                    (string-append directory "/widget.el.gz"))
         (let* ((library (index-standard-library
                          #:dialect 'elisp
                          #:roots (list directory)))
                (sites (stdlib-definitions library "widget-size"))
                (site (and (pair? sites) (car sites))))
           (assert-equal #:expect 1 #:got (length sites)
             #:name "the compressed definition was read")
           (assert-equal #:expect "widget-size"
             #:got (and site (definition-name (caddr site))))
           (assert-true (and site (string-suffix? "/widget.el.gz" (car site)))
             #:name "the answer names the file it is really in")))))

    (test "a name the library does not define answers nothing"
      (with-temp-directory
       (lambda (directory)
         (spit (string-append directory "/base.scm") "(define x 1)\n")
         (assert-equal #:expect '()
           #:got (stdlib-definitions (library-of directory) "elsewhere")))))

    (test "the library offers the names it defines"
      (with-temp-directory
       (lambda (directory)
         (spit (string-append directory "/base.scm")
               "(define (make-widget size) size)\n(define default-size 3)\n")
         (assert-equal #:expect (list "default-size" "make-widget")
           #:got (sort (names-of (library-of directory #:builtins ""))
                       string<?)))))

    (test "a file inside another root is indexed once"
      (with-temp-directory
       (lambda (directory)
         (let ((inner (string-append directory "/inner")))
           (spit (string-append inner "/base.scm") "(define (only-one) 1)\n")
           (let ((library (index-standard-library
                           #:dialect 'scheme
                           #:roots (list directory inner))))
             (assert-equal #:expect 1
               #:got (length (stdlib-definitions library "only-one"))))))))

    (test "a directory that is not there is an empty library, not an error"
      (let ((library (index-standard-library
                      #:dialect 'scheme
                      #:roots (list "/nonexistent-sift-stdlib"))))
        (assert-equal #:expect '() #:got (stdlib-definitions library "car"))
        (assert-equal #:expect '() #:got (stdlib-names library))))

    (test "a source URL is fetched only when download-sources is on"
      (with-temp-directory
       (lambda (directory)
         (let* ((package (string-append directory "/pkg"))
                (archive (string-append directory "/pkg.tar.gz"))
                (cache (string-append directory "/cache"))
                (url (string-append "file://" archive)))
           (spit (string-append package "/a.scm") "(define (outside) 1)\n")
           (assert-equal #:expect 0
             #:got (system* "tar" "-czf" archive "-C" directory "pkg"))
           (let ((previous (getenv "XDG_CACHE_HOME")))
             (dynamic-wind
               (lambda () (setenv "XDG_CACHE_HOME" cache))
               (lambda ()
                 (stdlib-install! (list url) #:download? #f)
                 (assert-equal #:expect '() #:got (resolve-configured-roots!)
                   #:name "the URL is not fetched without the setting")
                 (stdlib-install! (list url) #:download? #t)
                 (let ((roots (resolve-configured-roots!)))
                   (assert-equal #:expect 1 #:got (length roots)
                     #:name "the URL is fetched when the setting is on")
                   (assert-true
                     (and (pair? roots)
                       (file-exists? (string-append (car roots) "/pkg/a.scm")))
                     #:name "and the tree it unpacked is the root")))
               (lambda ()
                 (stdlib-install! #f)
                 (if previous
                   (setenv "XDG_CACHE_HOME" previous)
                   (unsetenv "XDG_CACHE_HOME")))))))))

    ;; ===== the directories a library is in =====

    (test "the Emacs discovery script looks in the third-party package trees"
      ;; `-Q' runs no init file, so the elpa packages are not on load-path
      ;; unless the script puts them there, and a package nobody can navigate
      ;; to is a package the tool does not know about.
      (let ((script ((module-ref (resolve-module '(sift stdlib))
                                 'dialect-script)
                     'elisp)))
        (assert-contains #:expect script #:got "package-user-dir")
        (assert-contains #:expect script #:got "site-lisp"))
      (assert-contains #:expect ((module-ref (resolve-module '(sift stdlib))
                                             'dialect-script)
                                 'scheme)
        #:got "local/share/guile"))

    (test "Common Lisp looks where Quicklisp installs its systems"
      (with-temp-directory
       (lambda (directory)
         (let ((software (string-append directory
                                        "/quicklisp/dists/quicklisp/software")))
           (make-directories software)
           (let ((previous (getenv "HOME")))
             (dynamic-wind
               (lambda () (setenv "HOME" directory))
               (lambda ()
                 (assert-true (and (member software
                                           ((module-ref (resolve-module '(sift stdlib))
                                                        'common-lisp-roots)))
                                #t)
                   #:name "the Quicklisp software directory is a root"))
               (lambda ()
                 (if previous
                   (setenv "HOME" previous)
                   (unsetenv "HOME")))))))))

    ;; ===== Racket, which ships its library as source =====

    (test "a Racket library is read from the sources the installation ships"
      ;; there is no Racket program to ask, so the files are the whole answer:
      ;; what a `#lang' module defines is what the index holds.
      (with-temp-directory
       (lambda (directory)
         (spit (string-append directory "/base.rkt")
               (string-append "#lang racket/base\n(provide make-widget)\n\n"
                              "(define (make-widget size) (* size 2))\n\n"
                              "(struct posn (x y) #:transparent)\n"))
         (let ((library (index-standard-library #:dialect 'racket
                                                #:roots (list directory))))
           (assert-equal #:expect (list "make-widget" "posn")
             #:got (sort (names-of library) string<?)
             #:name "the names the module defines, and not its `#lang' line")
           (assert-equal #:expect 'function
             #:got (definition-kind
                    (caddr (car (stdlib-definitions library "make-widget")))))
           (assert-equal #:expect 'struct
             #:got (definition-kind
                    (caddr (car (stdlib-definitions library "posn")))))))))

    (test "a Racket installation's own sources are the roots, and are not asked for"
      (with-temp-directory
       (lambda (directory)
         (let ((prefix (string-append directory "/prefix"))
               (home (string-append directory "/home"))
               (collects (string-append directory "/extra-collects")))
           (make-directories (string-append prefix "/bin"))
           (make-directories (string-append prefix "/share/racket/collects"))
           (make-directories (string-append prefix "/share/racket/pkgs"))
           (make-directories (string-append home "/.local/share/racket/8.12/collects"))
           (make-directories collects)
           (spit (string-append prefix "/bin/racket") "#!/bin/sh\n")
           (let ((previous-path (getenv "PATH"))
                 (previous-home (getenv "HOME"))
                 (previous-collects (getenv "PLTCOLLECTS")))
             (dynamic-wind
               (lambda ()
                 (setenv "PATH" (string-append prefix "/bin:" previous-path))
                 (setenv "HOME" home)
                 (setenv "PLTCOLLECTS" collects))
               (lambda ()
                 (let ((roots ((module-ref (resolve-module '(sift stdlib))
                                           'racket-roots))))
                   (assert-true
                     (and (root-ending roots "prefix/share/racket/collects") #t)
                     #:name "the collects the installation ships")
                   (assert-true
                     (and (root-ending roots "prefix/share/racket/pkgs") #t)
                     #:name "the pkgs installed beside it")
                   (assert-true
                     (and (root-ending roots
                                       "home/.local/share/racket/8.12/collects")
                       #t)
                     #:name "a user-scope package tree")
                   (assert-true (and (root-ending roots "extra-collects") #t)
                     #:name "and a directory `PLTCOLLECTS' names")))
               (lambda ()
                 (setenv "PATH" previous-path)
                 (if previous-home
                   (setenv "HOME" previous-home)
                   (unsetenv "HOME"))
                 (if previous-collects
                   (setenv "PLTCOLLECTS" previous-collects)
                   (unsetenv "PLTCOLLECTS")))))))))

    ;; ===== the summary =====

    (test "the summary counts the names the library holds"
      (with-temp-directory
       (lambda (directory)
         (spit (string-append directory "/scheme-builtins.scm") generated-source)
         (let ((library (library-of directory
                                    #:builtins (string-append
                                                directory "/scheme-builtins.scm"))))
           (assert-equal #:expect "scheme: unknown, 3 names"
             #:got (stdlib-summary library))))))))
