(define (make-adder n)
  (lambda (x) (+ x n)))

(define add10 (make-adder 10))
(display (add10 7))
(newline)
