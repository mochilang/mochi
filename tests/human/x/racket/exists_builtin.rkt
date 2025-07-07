#lang racket
(define data '(1 2))
(define flag (ormap (lambda (x) (= x 1)) data))
(displayln flag)
