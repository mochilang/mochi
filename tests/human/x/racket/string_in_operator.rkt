#lang racket
(define s "catch")
(displayln (regexp-match? #"cat" s))
(displayln (regexp-match? #"dog" s))
