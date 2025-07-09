#lang racket
(require racket/list)
(define matrix '('(1 2) '(3 4)))
(set! matrix (list-set matrix 1 (list-set (list-ref matrix 1) 0 5)))
(displayln (if (string? (if (string? matrix) (string-ref matrix 1) (list-ref matrix 1))) (string-ref (if (string? matrix) (string-ref matrix 1) (list-ref matrix 1)) 0) (list-ref (if (string? matrix) (string-ref matrix 1) (list-ref matrix 1)) 0)))
