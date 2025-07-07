#lang racket
(require racket/list)
(define xs '(1 2 3))
(define (in-list? x lst) (if (member x lst) #t #f))
(displayln (in-list? 2 xs))
(displayln (not (in-list? 5 xs)))
