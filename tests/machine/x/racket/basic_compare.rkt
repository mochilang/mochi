#lang racket
(define a (- 10 3))
(define b (+ 2 2))
(displayln a)
(displayln (equal? a 7))
(displayln (cond [(string? b) (string<? b 5)] [(string? 5) (string<? b 5)] [else (< b 5)]))
