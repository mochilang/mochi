#lang racket
(require racket/list)
(define xs (list 10 20 30))
(displayln (cond [(string? xs) (string-ref xs 1)] [(hash? xs) (hash-ref xs 1)] [else (list-ref xs 1)]))
