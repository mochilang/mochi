#lang racket
(define x 2)
(define label (match x [1 "one"] [2 "two"] [3 "three"] [_ "unknown"]))
(displayln label)
