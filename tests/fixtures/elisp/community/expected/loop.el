;;; loop.el --- a demo -*- lexical-binding: t; -*-

(defun sift-issue-nine (s)
  "Ask of a LOOP what its clauses already say."
  (loop for c across s
        for pointer :of-type fixnum = 0 then (1+ pointer)
        with res :of-type list = nil
        do (push c res)))

(defun sift-issue-nine-headless (n)
  "A LOOP written with its clauses on their own lines."
  (loop
    for i below n
    do (message "%s" i)))

(provide 'loop-demo)
