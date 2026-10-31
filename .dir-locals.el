;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>
;;; .dir-locals.el --- per-directory settings for sift  -*- lexical-binding: t; -*-
;;
;; The Emacs package lives in emacs/ and is loaded by the tests with -L
;; emacs; these settings make editing this repository with `sift-mode'
;; behave the way the project does.

((emacs-lisp-mode
  . ((sift-executable . "./bin/sift")
     (sift-dialect . "elisp")))
 (scheme-mode
  . ((sift-executable . "./bin/sift")
     (sift-dialect . "scheme")))
 (nil
  . ((fill-column . 96))))
