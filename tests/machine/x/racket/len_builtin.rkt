#lang racket
(displayln (cond [(string? (list 1 2 3)) (string-length (list 1 2 3))] [(hash? (list 1 2 3)) (hash-count (list 1 2 3))] [else (length (list 1 2 3))]))
