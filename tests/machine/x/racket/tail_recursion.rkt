#lang racket
(define (sum_rec n acc)
  (let/ec return
(if (equal? n 0)
  (begin
(return acc)
  )
  (void)
)
(return (sum_rec (- n 1) (+ acc n)))
  ))
(displayln (sum_rec 10 0))
