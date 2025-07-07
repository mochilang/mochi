#lang racket
(define (boom a b)
  (displayln "boom")
  #t)
(displayln (and #f (boom 1 2)))
(displayln (or #t (boom 1 2)))
