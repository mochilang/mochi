#lang racket
(define numbers '(1 2 3))
(define avg (/ (apply + numbers) (length numbers)))
(displayln avg)
