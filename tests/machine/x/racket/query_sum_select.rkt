#lang racket
(define nums '(1 2 3))
(define result (for/sum ([n nums] #:when (and (> n 1))) n))
(displayln result)
