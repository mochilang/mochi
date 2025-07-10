#lang racket
(require racket/list)
(define s "mochi")
(displayln (cond [(string? s) (string-ref s 1)] [(hash? s) (hash-ref s 1)] [else (list-ref s 1)]))
