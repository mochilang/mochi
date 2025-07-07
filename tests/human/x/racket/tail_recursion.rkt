#lang racket
(define (sum-rec n acc)
  (if (zero? n)
      acc
      (sum-rec (- n 1) (+ acc n))))
(displayln (sum-rec 10 0))
