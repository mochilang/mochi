#lang racket
(define items (list (hash 'n 1 'v "a")
                    (hash 'n 1 'v "b")
                    (hash 'n 2 'v "c")))
(define sorted (sort items < #:key (lambda (h) (hash-ref h 'n))))
(define result (map (lambda (h) (hash-ref h 'v)) sorted))
(displayln result)
