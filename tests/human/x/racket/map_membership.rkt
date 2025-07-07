#lang racket
(define m (hash 'a 1 'b 2))
(displayln (hash-has-key? m 'a))
(displayln (hash-has-key? m 'c))
