#lang racket
(define m (hash "a" 1 "b" 2))
(for ([k (if (hash? m) (hash-keys m) m)])
(displayln k)
)
