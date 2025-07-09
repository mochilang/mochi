#lang racket
(require racket/list)
(define data (hash "outer" (hash "inner" 1)))
(set! data (list-set data "outer" (hash-set (list-ref data "outer") "inner" 2)))
(displayln (hash-ref (hash-ref data "outer") "inner"))
