#lang racket
(define m (hash 1 "a" 2 "b"))
(displayln (if (string? m) (string-ref m 1) (list-ref m 1)))
