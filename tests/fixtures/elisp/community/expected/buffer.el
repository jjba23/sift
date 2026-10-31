;;; buffer.el --- a demo -*- lexical-binding: t; -*-

(defun my-greet (name)
  "Greet NAME."
  (message "Hello, %s" name))

(let ((x 1)
      (y 2))
  (+ x y))

(provide 'buffer)
