#lang racket
(require racket/list)
(define xs (list 1 2 3))
(displayln (cond [(string? xs) (regexp-match? (regexp 2) xs)] [(hash? xs) (hash-has-key? xs 2)] [else (member 2 xs)]))
(displayln (not (cond [(string? xs) (regexp-match? (regexp 5) xs)] [(hash? xs) (hash-has-key? xs 5)] [else (member 5 xs)])))
