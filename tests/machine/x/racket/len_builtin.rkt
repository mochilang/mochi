#lang racket
(displayln (if (string? '(1 2 3)) (string-length '(1 2 3)) (length '(1 2 3))))
