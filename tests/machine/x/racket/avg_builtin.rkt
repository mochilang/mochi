#lang racket
(displayln (let ([xs '(1 2 3)]) (/ (apply + xs) (length xs))))
