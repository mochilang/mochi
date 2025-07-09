#lang racket
(define (sum3 a b c)
  (let/ec return
(return (+ (+ a b) c))
  ))
(displayln (sum3 1 2 3))
