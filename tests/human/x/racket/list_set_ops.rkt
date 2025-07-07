#lang racket
(define (union a b) (remove-duplicates (append a b)))
(define (except a b) (filter (lambda (x) (not (member x b))) a))
(define (intersect a b) (filter (lambda (x) (member x b)) a))
(displayln (union '(1 2) '(2 3)))
(displayln (except '(1 2 3) '(2)))
(displayln (intersect '(1 2 3) '(2 4)))
(displayln (length (append '(1 2) '(2 3))))
