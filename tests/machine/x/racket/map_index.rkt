#lang racket
(require racket/list)
(define m (hash "a" 1 "b" 2))
(displayln (cond [(string? m) (string-ref m "b")] [(hash? m) (hash-ref m "b")] [else (list-ref m "b")]))
