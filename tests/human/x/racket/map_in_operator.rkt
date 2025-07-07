#lang racket
(define m (hash 1 "a" 2 "b"))
(displayln (hash-has-key? m 1))
(displayln (hash-has-key? m 3))
