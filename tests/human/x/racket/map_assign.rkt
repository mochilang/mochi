#lang racket
(define scores (hash 'alice 1))
(hash-set! scores 'bob 2)
(displayln (hash-ref scores 'bob))
