#lang racket
(define (boom )
  (let/ec return
(displayln "boom")
(return #t)
  ))
(displayln (and (and (< 1 2) (< 2 3)) (< 3 4)))
(displayln (and (and (< 1 2) (> 2 3)) (boom )))
(displayln (and (and (and (< 1 2) (< 2 3)) (> 3 4)) (boom )))
