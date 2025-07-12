#lang racket
(define nums '(1 2 3))
(define result (for/sum ([n nums] #:when (and (cond [(string? n) (string>? n 1)] [(string? 1) (string>? n 1)] [else (> n 1)]))) n))
(displayln result)
