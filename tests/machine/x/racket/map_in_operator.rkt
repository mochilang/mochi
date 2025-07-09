#lang racket
(define m (hash 1 "a" 2 "b"))
(displayln (if (member 1 m) #t #f))
(displayln (if (member 3 m) #t #f))
