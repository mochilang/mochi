#lang racket
(define xs (list 1 2 3))
(displayln (if (hash? xs) (hash-has-key? xs 2) (if (member 2 xs) #t #f)))
(displayln (not (if (hash? xs) (hash-has-key? xs 5) (if (member 5 xs) #t #f))))
