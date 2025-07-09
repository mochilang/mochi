#lang racket
(require racket/list)
(define m (hash "a" 1 "b" 2))
(displayln (cond [(string? m) (regexp-match? (regexp "a") m)] [(hash? m) (hash-has-key? m "a")] [else (member "a" m)]))
(displayln (cond [(string? m) (regexp-match? (regexp "c") m)] [(hash? m) (hash-has-key? m "c")] [else (member "c" m)]))
