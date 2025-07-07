#lang racket
(define x 3)
(define y 4)
(define m (hash 'a x 'b y))
(displayln (list (hash-ref m 'a) (hash-ref m 'b)))
