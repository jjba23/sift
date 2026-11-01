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
;; Veritas is never installed here.  The suite loads it from a checkout --
;; VERITAS_DIR, ~/Hacking/veritas by default -- because the checkout under test
;; is the one being developed.  A manifest that also provided a packaged copy
;; would put two of them on the load path with nothing saying which one a test
;; ran against, which is exactly the confusion a test rig cannot afford.

(use-modules (gnu packages emacs)
             (gnu packages guile)
             (gnu packages guile-xyz))

(packages->manifest
 (list (specification->package "make")
       guile-3.0
       guile-fibers
       guile-json-4
       guile-veritas
       ;; the Emacs package is developed and tested here too; a minimal
       ;; Emacs has ERT, Flymake and `json', which is all it needs
       emacs-minimal))
