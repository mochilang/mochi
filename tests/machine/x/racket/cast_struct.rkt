#lang racket
(struct Todo (title) #:transparent)
(define todo (Todo (hash-ref (hash "title" "hi") 'title)))
(displayln (hash-ref todo 'title))
