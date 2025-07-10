#lang racket
(require racket/list)
(define scores (hash "alice" 1))
(set! scores (cond [(hash? scores) (hash-set scores "bob" 2)] [else (list-set scores "bob" 2)]))
(displayln (cond [(string? scores) (string-ref scores "bob")] [(hash? scores) (hash-ref scores "bob")] [else (list-ref scores "bob")]))
