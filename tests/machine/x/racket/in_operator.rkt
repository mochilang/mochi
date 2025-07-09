#lang racket
(define xs '(1 2 3))
(displayln (if (member 2 xs) #t #f))
(displayln (not (if (member 5 xs) #t #f)))
