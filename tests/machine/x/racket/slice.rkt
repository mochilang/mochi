#lang racket
(displayln (if (string? (list 1 2 3)) (substring (list 1 2 3) 1 3) (take (drop (list 1 2 3) 1) (- 3 1))))
(displayln (if (string? (list 1 2 3)) (substring (list 1 2 3) 0 2) (take (drop (list 1 2 3) 0) (- 2 0))))
(displayln (if (string? "hello") (substring "hello" 1 4) (take (drop "hello" 1) (- 4 1))))
