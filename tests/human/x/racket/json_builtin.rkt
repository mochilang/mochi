#lang racket
(require json)
(define m (hash 'a 1 'b 2))
(displayln (jsexpr->string m))
