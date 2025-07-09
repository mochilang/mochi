#lang racket
(displayln (remove-duplicates (append (list 1 2) (list 2 3))))
(displayln (filter (lambda (x) (not (member x (list 2)))) (list 1 2 3)))
(displayln (filter (lambda (x) (member x (list 2 4))) (list 1 2 3)))
(displayln (cond [(string? (append (list 1 2) (list 2 3))) (string-length (append (list 1 2) (list 2 3)))] [(hash? (append (list 1 2) (list 2 3))) (hash-count (append (list 1 2) (list 2 3)))] [else (length (append (list 1 2) (list 2 3)))]))
