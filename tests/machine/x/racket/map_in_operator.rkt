#lang racket
(define m (hash 1 "a" 2 "b"))
(displayln (if (hash? m) (hash-has-key? m 1) (if (member 1 m) #t #f)))
(displayln (if (hash? m) (hash-has-key? m 3) (if (member 3 m) #t #f)))
