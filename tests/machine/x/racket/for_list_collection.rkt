#lang racket
(for ([n (if (hash? '(1 2 3)) (hash-keys '(1 2 3)) '(1 2 3))])
(displayln n)
)
