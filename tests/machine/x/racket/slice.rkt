#lang racket
(require racket/list)
(displayln (cond [(string? (list 1 2 3)) (substring (list 1 2 3) 1 3)] [(hash? (list 1 2 3)) (hash-ref (list 1 2 3) 1)] [else (take (drop (list 1 2 3) 1) (- 3 1))]))
(displayln (cond [(string? (list 1 2 3)) (substring (list 1 2 3) 0 2)] [(hash? (list 1 2 3)) (hash-ref (list 1 2 3) 0)] [else (take (drop (list 1 2 3) 0) (- 2 0))]))
(displayln (cond [(string? "hello") (substring "hello" 1 4)] [(hash? "hello") (hash-ref "hello" 1)] [else (take (drop "hello" 1) (- 4 1))]))
