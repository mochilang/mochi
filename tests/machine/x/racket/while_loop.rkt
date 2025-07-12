#lang racket
(define i 0)
(let/ec break
  (let loop ()
    (when (< i 3)
      (let/ec continue
(displayln i)
(set! i (+ i 1))
      )
      (loop)))
)
