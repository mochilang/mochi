(define (sum3 a b c)
  (call/cc (lambda (return)
    (return (+ (+ a b) c))
  ))
)

(begin (display (sum3 1 2 3)) (newline))
