#lang racket
(define (outer x)
(define (inner y)
  (+ x y)
)
  (inner 5)
)
(displayln (outer 3))
