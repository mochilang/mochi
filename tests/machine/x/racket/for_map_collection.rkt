#lang racket
(define m (hash "a" 1 "b" 2))
(for ([k m])
(displayln k)
)
