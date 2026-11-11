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
;; are.  Neither needs Guile, Emacs or SBCL to be installed to be tested,
;; which is the point of keeping the parsing away from the discovery: the half
;; that reads a format is the half that can be wrong, and it is testable with
;; a string.

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

(define builtins-text
  (string-join
   '("sift\tversion\t3.0.11"
     "sift\troot\t/usr/share/guile/3.0"
     "### car\tfunction\t(car pair)"
     "    Return the contents of the car field of pair."
     ""
     "### cons\tfunction\t(cons x y)"
     "    Return a new pair whose car is x and whose cdr is y."
     ""
     "### native-array-ref\tsomething\t(native-array-ref array k)"
     "    No kind sift has a word for.")
   "\n"))

(define* (library-of directory #:key (builtins builtins-text))
  "Return a standard library whose sources are the files under DIRECTORY."
  (index-standard-library #:dialect 'scheme
                          #:roots (list directory)
                          #:builtins-text builtins
                          #:builtins-path (string-append directory "/builtins.txt")))

(define (names-of library)
  (map car (stdlib-names library)))

;;; ===== The generated index =====

(define (spec)
  (suite "stdlib: the standard library on disk"

    (test "an entry names a builtin, its kind, its argument list and its prose"
      (let ((entries (parse-builtins builtins-text)))
        (assert-equal #:expect (list "car" "cons" "native-array-ref")
          #:got (map builtin-name entries))))

    (test "a procedure is a function and carries its signature"
      (let ((car* (find (lambda (builtin) (string=? (builtin-name builtin) "car"))
                        (parse-builtins builtins-text))))
        (assert-equal #:expect 'function #:got (builtin-kind car*))
        (assert-equal #:expect "(car pair)" #:got (builtin-signature car*))))

    (test "the prose of an entry is the paragraph under it"
      (let ((cons* (find (lambda (builtin) (string=? (builtin-name builtin) "cons"))
                         (parse-builtins builtins-text))))
        (assert-equal #:expect "Return a new pair whose car is x and whose cdr is y."
          #:got (builtin-documentation cons*))))

    (test "an entry's position is the name written in it"
      (let ((car* (car (parse-builtins builtins-text))))
        (assert-equal #:expect "car"
          #:got (substring builtins-text (builtin-start car*) (builtin-end car*)))))

    (test "a kind with no word of its own is a variable"
      (let ((ref (find (lambda (builtin)
                         (string=? (builtin-name builtin) "native-array-ref"))
                       (parse-builtins builtins-text))))
        (assert-equal #:expect 'variable #:got (builtin-kind ref))))

    (test "a file with no entries has no builtins"
      (assert-equal #:expect '() #:got (parse-builtins "")))

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
                          #:roots (list directory)
                          #:builtins-text ""))
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
                           #:roots (list directory inner)
                           #:builtins-text "")))
             (assert-equal #:expect 1
               #:got (length (stdlib-definitions library "only-one"))))))))

    (test "a directory that is not there is an empty library, not an error"
      (let ((library (index-standard-library
                      #:dialect 'scheme
                      #:roots (list "/nonexistent-sift-stdlib")
                      #:builtins-text "")))
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

    ;; ===== the builtins =====

    (test "a builtin is defined where the generated index writes it"
      (let* ((library (index-standard-library #:dialect 'scheme
                                              #:roots '()
                                              #:builtins-text builtins-text))
             (sites (stdlib-definitions library "car"))
             (site (and (pair? sites) (car sites)))
             (definition (and site (caddr site))))
        (assert-equal #:expect 1 #:got (length sites))
        (assert-equal #:expect "car" #:got (definition-name definition))
        ;; a builtin is not a form in a file: what it has is a place in the
        ;; generated index and the documentation written there
        (assert-equal #:expect #f #:got (definition-form definition))
        (assert-equal #:expect "car"
          #:got (substring (cadr site) (definition-index definition)
                           (definition-end definition)))))

    (test "the documentation of a builtin is the prose beside it"
      (let ((library (index-standard-library #:dialect 'scheme
                                             #:roots '()
                                             #:builtins-text builtins-text)))
        (assert-contains #:expect (stdlib-builtin-markdown library "car")
          #:got "Return the contents of the car field of pair.")))

    (test "a builtin nobody documents is not offered"
      (let ((library (index-standard-library #:dialect 'scheme
                                             #:roots '()
                                             #:builtins-text builtins-text)))
        (assert-equal #:expect #f
          #:got (stdlib-builtin-markdown library "nonesuch"))))))
