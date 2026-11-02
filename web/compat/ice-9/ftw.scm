;; sift --- `(ice-9 ftw)' for a WebAssembly build
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `(sift imports)' imports this library for one procedure: `scandir', which
;; it uses to walk a project looking for the libraries the project declares.
;; A browser tab has no project and no directories, so the walk finds nothing
;; -- and `(sift imports)' asks `scandir' only inside a `cond-expand' arm that
;; the WebAssembly build does not compile.  What is left is the import, which
;; has to resolve for the module to compile at all.
;;
;; So this file answers the empty list, and nothing else.  The rest of ftw --
;; `ftw', `nftw', `file-system-fold', `file-system-tree', `find-files' -- is
;; deliberately absent: a procedure that walked nothing under a name that
;; promises a walk would be worse than an unbound name at compile time.

(define-library (ice-9 ftw)
  (export scandir)
  (import (scheme base))

  (begin
    (define (scandir directory . rest)
      "Return the empty list: a browser has no directories to list."
      '())))
