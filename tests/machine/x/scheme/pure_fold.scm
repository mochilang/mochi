(define (triple x)
  (call/cc (lambda (return)
    (return (* x 3))
  ))
)

(begin (display (triple (+ 1 2))) (newline))
