#lang racket
(require racket/list)
(define m (hash 1 "a" 2 "b"))
(displayln (cond [(string? m) (string-ref m 1)] [(hash? m) (hash-ref m 1)] [else (list-ref m 1)]))
