#lang racket
(struct Todo (title) #:transparent #:mutable)
(define todo (Todo (hash-ref (hash "title" "hi") "title")))
(displayln (Todo-title todo))
