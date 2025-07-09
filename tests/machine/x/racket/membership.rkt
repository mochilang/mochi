#lang racket
(define nums '(1 2 3))
(displayln (if (member 2 nums) #t #f))
(displayln (if (member 4 nums) #t #f))
