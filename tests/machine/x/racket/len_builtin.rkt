#lang racket
(displayln (cond [(string? '(1 2 3)) (string-length '(1 2 3))] [(hash? '(1 2 3)) (hash-count '(1 2 3))] [else (length '(1 2 3))]))
