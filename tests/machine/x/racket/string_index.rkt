#lang racket
(define s "mochi")
(displayln (if (string? s) (string-ref s 1) (list-ref s 1)))
