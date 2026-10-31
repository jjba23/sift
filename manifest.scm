;; sift --- a dialect-aware formatter and linter for Scheme, Emacs Lisp and
;; Common Lisp.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The development environment: Guile, the fibers and JSON libraries Veritas
;; needs, and make.  Veritas itself is loaded from a checkout
;; (VERITAS_DIR, ~/Hacking/veritas by default) rather than installed, so the
;; suite always runs against the version being developed; the packaged
;; release is defined below for anyone who would rather pin it.

(use-modules (guix packages)
             (guix utils)
             (guix git-download)
             (gnu packages emacs)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (guix build-system guile)
             ((guix licenses) #:prefix license:))

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
specific language for test suites: suites, tests, assertions, matchers,
table-driven cases, mocks, snapshots, generators and coverage.")
    (license license:lgpl3+)))

(packages->manifest
 (list (specification->package "make")
       guile-3.0
       guile-fibers
       guile-json-4
       ;; the Emacs package is developed and tested here too; a minimal
       ;; Emacs has ERT, Flymake and `json', which is all it needs
       emacs-minimal))
