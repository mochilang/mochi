#lang racket
(displayln (if (string? (hash "a" 1 "b" 2)) (string-length (hash "a" 1 "b" 2)) (length (hash "a" 1 "b" 2))))
