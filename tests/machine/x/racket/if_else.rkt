#lang racket
(define x 5)
(if (cond [(string? x) (string>? x 3)] [(string? 3) (string>? x 3)] [else (> x 3)])
  (begin
(displayln "big")
  )
  (begin
(displayln "small")
  )
)
