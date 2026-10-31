(define (square x)
  (* x x))

(define (classify n)
  (cond ((< n 0) 'negative)
    ((= n 0) 'zero)
    (else 'positive)))

(let ((a 1)
      (b 2))
  (+ a b (* a b)))
