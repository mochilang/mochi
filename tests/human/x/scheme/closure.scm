(define (makeAdder n)
  (lambda (x) (+ x n)))

(define add10 (makeAdder 10))
(begin (display (add10 7)) (newline))
