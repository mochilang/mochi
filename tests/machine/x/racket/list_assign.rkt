#lang racket
(require racket/list)
(define nums '(1 2))
(set! nums (cond [(hash? nums) (hash-set nums 1 3)] [else (list-set nums 1 3)]))
(displayln (cond [(string? nums) (string-ref nums 1)] [(hash? nums) (hash-ref nums 1)] [else (list-ref nums 1)]))
