#lang racket
(define xs '(10 20 30))
(displayln (if (string? xs) (string-ref xs 1) (list-ref xs 1)))
