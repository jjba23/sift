;;; use-package.el --- a demo -*- lexical-binding: t; -*-

(use-package evil
:ensure t
:init
(setq evil-want-integration t)
:config
(evil-mode 1))

(use-package magit
:ensure t
:bind
("C-x g" . magit-status))

(use-package foo
:ensure t
:custom
(foo-directory "/tmp/foo")
:hook
(prog-mode . foo-mode))

(provide 'use-package-demo)
