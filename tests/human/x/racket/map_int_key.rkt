#lang racket
(define m (hash 1 "a" 2 "b"))
(displayln (hash-ref m 1))
