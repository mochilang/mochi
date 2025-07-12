#lang racket
(require racket/list)
(displayln (cond [(string? '(1 2 3)) (substring '(1 2 3) 1 3)] [(hash? '(1 2 3)) (hash-ref '(1 2 3) 1)] [else (take (drop '(1 2 3) 1) (- 3 1))]))
(displayln (cond [(string? '(1 2 3)) (substring '(1 2 3) 0 2)] [(hash? '(1 2 3)) (hash-ref '(1 2 3) 0)] [else (take (drop '(1 2 3) 0) (- 2 0))]))
(displayln (cond [(string? "hello") (substring "hello" 1 4)] [(hash? "hello") (hash-ref "hello" 1)] [else (take (drop "hello" 1) (- 4 1))]))
