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

(defun sift-issue-nine-bodies (xs)
  "A LOOP whose clause bodies start their own lines."
  (loop for x in xs
        do
        (message "%s" x)
        (message "%s" x))
  (loop for x in xs
        if (> x 2)
        do
        (message "%s" x)))

(provide 'loop-demo)
