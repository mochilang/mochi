#lang racket
(define m (hash "a" 1 "b" 2))
(displayln (regexp-match? (regexp "a") m))
(displayln (regexp-match? (regexp "c") m))
