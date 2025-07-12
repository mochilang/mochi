#lang racket
(define x 12)
(define msg (if (cond [(string? x) (string>? x 10)] [(string? 10) (string>? x 10)] [else (> x 10)]) "yes" "no"))
(displayln msg)
