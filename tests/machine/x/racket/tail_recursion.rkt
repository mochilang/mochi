#lang racket
(define (sum_rec n acc)
(if (= n 0)
  (begin
acc
  )
  (void)
)
(sum_rec (- n 1) (+ acc n))
)
(displayln (sum_rec 10 0))
