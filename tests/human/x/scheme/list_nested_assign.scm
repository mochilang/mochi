(define matrix (vector (vector 1 2) (vector 3 4)))
(vector-set! (vector-ref matrix 1) 0 5)
(display (vector-ref (vector-ref matrix 1) 0))
(newline)
