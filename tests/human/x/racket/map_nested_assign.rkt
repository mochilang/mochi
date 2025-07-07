#lang racket
(define data (hash 'outer (hash 'inner 1)))
(hash-set! (hash-ref data 'outer) 'inner 2)
(displayln (hash-ref (hash-ref data 'outer) 'inner))
