#lang racket
(define x 8)
(define msg (if (cond [(string? x) (string>? x 10)] [(string? 10) (string>? x 10)] [else (> x 10)]) "big" (if (cond [(string? x) (string>? x 5)] [(string? 5) (string>? x 5)] [else (> x 5)]) "medium" "small")))
(displayln msg)
