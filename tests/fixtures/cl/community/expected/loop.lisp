(defun print-each (n)
  (loop :for i :below n
        :do
        (print i)
        (finish-output)))

(defun collect-each (xs)
  (loop :for x :in xs
        :collect (list x
                       x)))
