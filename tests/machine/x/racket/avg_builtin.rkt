#lang racket
(displayln (let ([xs (list 1 2 3)]) (/ (apply + xs) (length xs))))
