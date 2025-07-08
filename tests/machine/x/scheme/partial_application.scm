(define (add a b)
  (call/cc (lambda (return)
    (return (+ a b))
  ))
)

(define add5 (add 5))
(begin (display (add5 3)) (newline))
