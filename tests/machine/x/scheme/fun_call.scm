(define (add a b)
  (call/cc (lambda (return)
    (return (+ a b))
  ))
)

(begin (display (add 2 3)) (newline))
