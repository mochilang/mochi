#lang racket
(define x 5)
(if (> x 3)
  (begin
(displayln "big")
  )
  (begin
(displayln "small")
  )
)
