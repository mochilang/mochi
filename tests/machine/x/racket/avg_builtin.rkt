#lang racket
(displayln (let ([xs '(1 2 3)] [n (length '(1 2 3))]) (if (= n 0) 0 (/ (for/fold ([s 0]) ([v xs]) (+ s v)) n))))
