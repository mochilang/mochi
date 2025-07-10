#lang racket
(define data (list 1 2))
(define flag (not (null? (for*/list ([x data] #:when (and (equal? x 1))) x))))
(displayln flag)
