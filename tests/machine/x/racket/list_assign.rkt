#lang racket
(require racket/list)
(define nums '(1 2))
(set! nums (list-set nums 1 3))
(displayln (if (string? nums) (string-ref nums 1) (list-ref nums 1)))
