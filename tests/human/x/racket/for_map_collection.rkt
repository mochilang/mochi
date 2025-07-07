#lang racket
(define m (hash "a" 1 "b" 2))
(for ([k (hash-keys m)])
  (displayln k))
