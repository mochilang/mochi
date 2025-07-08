#lang racket
(displayln (take (drop '(1 2 3) 1) (- 3 1)))
(displayln (take (drop '(1 2 3) 0) (- 2 0)))
(displayln (substring "hello" 1 4))
