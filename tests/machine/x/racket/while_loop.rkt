#lang racket
(define i 0)
(let loop ()
  (when (< i 3)
(displayln i)
(set! i (+ i 1))
    (loop)))
