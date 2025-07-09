#lang racket
(for ([n (if (hash? (list 1 2 3)) (hash-keys (list 1 2 3)) (list 1 2 3))])
(displayln n)
)
