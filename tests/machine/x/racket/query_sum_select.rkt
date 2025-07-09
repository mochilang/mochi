#lang racket
(define nums (list 1 2 3))
(define result (for*/list ([n nums] #:when (> n 1)) (apply + n)))
(displayln result)
