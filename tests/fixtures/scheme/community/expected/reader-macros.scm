'(a b)
`(c ,d ,@e)
#(1 2 3) #u8(1 2)
#;(ignored   form)
#| block |#
#+(or a b) (display "x")
(define table #(one two
                three))
(define bytes #vu8(1 2))
(define #{odd name}# 'x)
(define-syntax (m stx)
  (syntax-case stx ()
    [(_ x) #`(list #,x ,x)]))
(define (f lst' x'') (list lst' x''))
