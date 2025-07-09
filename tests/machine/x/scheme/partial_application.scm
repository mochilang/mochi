(define (add a b)
  (call/cc (lambda (return)
    (return (+ a b))
  ))
)

(define add5 (lambda (_p0) (add 5 _p0)))
(begin (display (add5 3)) (newline))
