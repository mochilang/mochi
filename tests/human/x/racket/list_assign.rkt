#lang racket
(define nums (vector 1 2))
(vector-set! nums 1 3)
(displayln (vector-ref nums 1))
