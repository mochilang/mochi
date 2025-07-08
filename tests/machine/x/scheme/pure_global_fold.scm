(define (inc x)
  (call/cc (lambda (return)
    (return (+ x k))
  ))
)

(define k 2)
(begin (display (inc 3)) (newline))
