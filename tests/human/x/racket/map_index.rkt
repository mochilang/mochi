#lang racket
(define m (hash 'a 1 'b 2))
(displayln (hash-ref m 'b))
