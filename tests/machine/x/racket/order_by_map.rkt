#lang racket
(define data (list (hash 'a 1 'b 2) (hash 'a 1 'b 1) (hash 'a 0 'b 5)))
(define sorted (for*/list ([x data]) x))
(displayln sorted)
