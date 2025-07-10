#lang racket
(define i 0)
(let/ec break
  (let loop ()
    (when (cond [(string? i) (string<? i 3)] [(string? 3) (string<? i 3)] [else (< i 3)])
      (let/ec continue
(displayln i)
(set! i (+ i 1))
      )
      (loop)))
)
