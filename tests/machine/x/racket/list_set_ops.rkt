#lang racket
(displayln (remove-duplicates (append '(1 2) '(2 3))))
(displayln (filter (lambda (x) (not (member x '(2)))) '(1 2 3)))
(displayln (filter (lambda (x) (member x '(2 4))) '(1 2 3)))
(displayln (cond [(string? (append '(1 2) '(2 3))) (string-length (append '(1 2) '(2 3)))] [(hash? (append '(1 2) '(2 3))) (hash-count (append '(1 2) '(2 3)))] [else (length (append '(1 2) '(2 3)))]))
