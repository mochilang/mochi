(define (outer x)
  (call/cc (lambda (return)
    (return (inner 5))
  ))
)

(begin (display (outer 3)) (newline))
