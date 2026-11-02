;; sift --- the WebAssembly entry point
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; What a Wasm host gets when it instantiates sift.wasm is the value of this
;; file's last expression: one procedure, `web-request', of one string to one
;; string.  That is the whole contract between the editor and the compiler.
;;
;; The editor does this, once, at load time:
;;
;;   const [sift] = await Scheme.load_main("sift.wasm");
;;   const answer = [...sift.call('{"op":"lint","source":"(if a b)"}')][0];
;;
;; and from then on every keystroke is one call and one JSON document.  No
;; ports are bound to the terminal, nothing is read from a filesystem, and the
;; program does not exit: a browser tab is not a process, and sift in one is a
;; library.
;;
;; The import list is deliberately the smallest that compiles: `(scheme base)'
;; is there for `values', and `(sift web)' is the facade -- which lives in this
;; directory and not in src/, and which is written in portable Scheme, so that
;; the Guile build of it, the one tests/web.scm drives, is the same code the
;; browser runs.

(import (scheme base)
  (sift web))

(values web-request)
