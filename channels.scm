;; sift
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Code:

(list (channel
       (name 'guix)
       (url "https://codeberg.org/guix/guix.git")
       (branch "master")
       (commit "b7c99ce1297f19da01ba26828464b5e6d9d74572")
       (introduction
        (make-channel-introduction "9edb3f66fd807b096b48283debdcddccfea34bad"
                                   (openpgp-fingerprint
                                    "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
       (name 'jointhefreeworld)
       (url "https://github.com/jjba23/guix-jointhefreeworld")
       (branch "trunk")
       ;; The release this configuration is built against: lambdock 0.21.1,
       ;; heks-toon 0.3.2, maak 0.14.0, veritas 0.9.0, scriba 0.4.5, sift
       ;; 1.11.1, and the rest of the collection, at the same revision.  The
       ;; sift bump is the release its own `maak release' cut: a declaration is
       ;; no longer read as a call (`use-modules`, `#:export`, a `defpackage`
       ;; and a parameter list all stop being counted as positional
       ;; arguments), `#:use-module` is an import the unused-import rule reads,
       ;; SIFT062 (`unused-definition`) names the definitions nothing can
       ;; reach, and 1.11.1 stops that import rule from reading a quasiquote as
       ;; a datum -- which is what let 1.11.0 talk this tree out of an import
       ;; its renderer needs.
       (commit "6832263d15081294222f600270506e58ccac2909")
       (introduction
        (make-channel-introduction
         "b0ac3f05075d7959860fc9e1db5871acddea3986"
         (openpgp-fingerprint
          "83BC 6E1C 8726 B8C2 97F8  D16E 24F4 6738 CE11 4AF6")))))
