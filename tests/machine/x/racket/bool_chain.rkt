#lang racket
(define (boom )
(displayln "boom")
#t
)
(displayln (and (and (< 1 2) (< 2 3)) (< 3 4)))
(displayln (and (and (< 1 2) (> 2 3)) (boom )))
(displayln (and (and (and (< 1 2) (< 2 3)) (> 3 4)) (boom )))
