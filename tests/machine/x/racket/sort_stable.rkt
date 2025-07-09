#lang racket
(define items (list (hash 'n 1 'v "a") (hash 'n 1 'v "b") (hash 'n 2 'v "c")))
(define result (for*/list ([i items]) (hash-ref i 'v)))
(displayln result)
