#lang racket
(define nums (list 1 2 3))
(displayln (if (hash? nums) (hash-has-key? nums 2) (if (member 2 nums) #t #f)))
(displayln (if (hash? nums) (hash-has-key? nums 4) (if (member 4 nums) #t #f)))
