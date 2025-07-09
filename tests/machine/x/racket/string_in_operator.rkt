#lang racket
(define s "catch")
(displayln (regexp-match? (regexp "cat") s))
(displayln (regexp-match? (regexp "dog") s))
