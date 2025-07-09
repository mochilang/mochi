#lang racket
(struct Counter (n) #:transparent #:mutable)
(define (inc c)
  (let/ec return
(set-Counter-n! c (+ (Counter-n c) 1))
  ))
(define c (Counter 0))
(inc c)
(displayln (Counter-n c))
