;; sift --- a dialect-aware formatter and linter for Scheme, Emacs Lisp and
;; Common Lisp.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; A package definition for the working tree.  Build or enter it with:
;;
;;   guix shell -f guix.scm -- sift version
;;
;; sift itself is a library plus a launcher, and depends only on Guile.
;; Veritas is a test-time dependency, so it is a native input rather than a
;; propagated one: installing sift on a machine that will never run its tests
;; should not install a test framework.

(use-modules (guix packages)
             (guix utils)
             (guix git-download)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (guix build-system guile)
             ((guix licenses) #:prefix license:))

(define %source-dir
  (dirname (current-filename)))

(define-public guile-veritas
  (package
    (name "guile-veritas")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/jjba23/veritas.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pr8yjnxkrp2fylmn8ihjapin7iz94jqi4hbc6yd0ajll11ajxc9"))))
    (build-system guile-build-system)
    (arguments
     (list #:source-directory "src"))
    (native-inputs (list guile-3.0))
    (propagated-inputs (list guile-fibers guile-json-4))
    (home-page "https://codeberg.org/jjba23/veritas")
    (synopsis "Testing framework for Guile")
    (description
     "Veritas is a testing framework for Guile with an embedded domain
specific language for test suites.")
    (license license:lgpl3+)))

(define-public sift
  (package
    (name "sift")
    (version "0.2.0")
    (source
     (local-file %source-dir
                 #:recursive? #t
                 #:select? (git-predicate %source-dir)))
    (build-system guile-build-system)
    (arguments
     (list #:source-directory "src"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-launcher
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin"))
                          (site (string-append out
                                               "/share/guile/site/3.0")))
                     (mkdir-p bin)
                     (install-file "bin/sift" bin)
                     (substitute* (string-append bin "/sift")
                       (("\\$here/../src")
                        site))))))))
    (native-inputs (list guile-veritas))
    (inputs (list guile-3.0))
    (home-page "https://codeberg.org/jjba23/sift")
    (synopsis "Dialect-aware formatter and linter for Scheme, Emacs Lisp and Common Lisp")
    (description
     "sift formats and lints Scheme, Emacs Lisp and Common Lisp without
losing a byte: it reads every comment, blank line and reader prefix into a
node of its own, so formatting is reversible and idempotent, and its
checkers are documented, independently testable and aware of which dialect
they are looking at.")
    (license license:gpl3+)))

sift
