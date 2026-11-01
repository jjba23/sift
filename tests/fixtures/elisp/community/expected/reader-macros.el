;;; reader-macros.el --- the read macros of the dialect -*- lexical-binding: t; -*-

(defun my-setup ()
  "Set up the table and the keys."
  (setq my-table #s(hash-table test equal data ("a" 1)))
  (setq my-cycle #1=(a . #1#))
  (setq my-keys [?\C-x ?\C-c]))

(provide 'reader-macros)
(setq my-rx (rx (| "a" "b")))
