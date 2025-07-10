#lang racket
(require racket/function)
(define (add a b)
  (+ a b)
)
(define add5 (curry add 5))
(displayln (add5 3))
