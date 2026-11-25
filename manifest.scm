;; sift --- a dialect-aware formatter and linter for Scheme, Emacs Lisp and
;; Common Lisp.
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The development environment: Guile, the fibers and JSON libraries Veritas
;; needs, and make.
;;
;; Veritas is the GitHub release pinned below, v1.6.0.  A bare `guile-veritas'
;; in this list resolved instead to the copy Guix carries, which is whatever the
;; Guix commit in channels.scm happens to hold -- 0.8.1 at the time of writing,
;; several minor releases behind the framework the suite is written against and
;; missing `lazy', which replaced the quoted `(comp ...)' the suite no longer
;; uses.

(use-modules (gnu packages)
             (gnu packages emacs)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (guix packages)
             (guix git-download)
             (guix build-system guile)
             ((guix licenses) #:prefix license:))

(define-public guile-veritas
  (package
    (name "guile-veritas")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jjba23/veritas.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lb80y2dks68a2jlr5xix9m8pyqnzwiqiaynbl05p463xxhzs0v7"))))
    (build-system guile-build-system)
    (arguments
     (list #:source-directory "src"))
    (native-inputs (list guile-3.0))
    (propagated-inputs (list guile-fibers guile-json-4))
    (home-page "https://github.com/jjba23/veritas")
    (synopsis "Testing framework for Guile")
    (description
     "Veritas is a testing framework for Guile with an embedded domain
specific language for defining test suites.  Emphasis is placed on legible,
maintainable tests: Veritas shuffles them and runs them concurrently by
default, and reports failures with the values that produced them.")
    (license license:expat)))

(packages->manifest
 (list (specification->package "make")
       guile-3.0
       guile-fibers
       guile-json-4
       guile-veritas
       ;; `maak docs' keeps `doc/rules.org' and, beside it, the API reference
       ;; dokgen extracts from the modules.
       (specification->package "guile-dokgen")
       (specification->package "texinfo")
       ;; the Emacs package is developed and tested here too; a minimal
       ;; Emacs has ERT, Flymake and `json', which is all it needs
       emacs-minimal))
