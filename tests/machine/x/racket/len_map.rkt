#lang racket
(displayln (cond [(string? (hash "a" 1 "b" 2)) (string-length (hash "a" 1 "b" 2))] [(hash? (hash "a" 1 "b" 2)) (hash-count (hash "a" 1 "b" 2))] [else (length (hash "a" 1 "b" 2))]))
