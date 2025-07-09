#lang racket
(define (makeAdder n)
  (let/ec return
(return (lambda (x) (+ x n)))
  ))
(define add10 (makeAdder 10))
(displayln (add10 7))
