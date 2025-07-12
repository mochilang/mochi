#lang racket
(require racket/list)
(define nums '(1 2 3))
(displayln (cond [(string? nums) (regexp-match? (regexp 2) nums)] [(hash? nums) (hash-has-key? nums 2)] [else (member 2 nums)]))
(displayln (cond [(string? nums) (regexp-match? (regexp 4) nums)] [(hash? nums) (hash-has-key? nums 4)] [else (member 4 nums)]))
