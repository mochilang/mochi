#lang racket
(define (makeAdder n)
(lambda (x) (+ x n))
)
(define add10 (makeAdder 10))
(displayln (add10 7))
