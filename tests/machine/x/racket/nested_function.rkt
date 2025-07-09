#lang racket
(define (outer x)
  (let/ec return
(define (inner y)
  (let/ec return
(return (+ x y))
  ))
(return (inner 5))
  ))
(displayln (outer 3))
