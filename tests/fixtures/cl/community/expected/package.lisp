(defpackage :demo
  (:use :cl)
  (:export #:run))

(in-package :demo)

(defun run (n)
  (loop :for i :below n
    :do (print i)))
