#lang racket
(define (boom a b)
  (let/ec return
(displayln "boom")
(return #t)
  ))
(displayln (and #f (boom 1 2)))
(displayln (or #t (boom 1 2)))
