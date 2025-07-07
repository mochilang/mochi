#lang racket
(struct counter (n) #:mutable)
(define (inc c)
  (set-counter-n! c (add1 (counter-n c))))
(define c (counter 0))
(inc c)
(displayln (counter-n c))
