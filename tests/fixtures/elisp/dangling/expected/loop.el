;;; loop.el --- a demo -*- lexical-binding: t; -*-

(defun sift-issue-nine-dangling (xs)
  "A LOOP in a project that puts its closers on their own line."
  (loop for x in xs
        do
        (message "%s" x)
  )
)
