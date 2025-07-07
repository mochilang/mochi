#lang racket
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add10 (make-adder 10))
(displayln (add10 7))
