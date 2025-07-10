#lang racket
(require racket/list)
(define s "catch")
(displayln (cond [(string? s) (regexp-match? (regexp "cat") s)] [(hash? s) (hash-has-key? s "cat")] [else (member "cat" s)]))
(displayln (cond [(string? s) (regexp-match? (regexp "dog") s)] [(hash? s) (hash-has-key? s "dog")] [else (member "dog" s)]))
