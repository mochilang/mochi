#lang racket
(require racket/list)
(define m (hash 1 "a" 2 "b"))
(displayln (cond [(string? m) (regexp-match? (regexp 1) m)] [(hash? m) (hash-has-key? m 1)] [else (member 1 m)]))
(displayln (cond [(string? m) (regexp-match? (regexp 3) m)] [(hash? m) (hash-has-key? m 3)] [else (member 3 m)]))
