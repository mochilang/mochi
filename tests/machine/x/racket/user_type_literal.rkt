#lang racket
(struct Person (name age) #:transparent #:mutable)
(struct Book (title author) #:transparent #:mutable)
(define book (Book "Go" (Person "Bob" 42)))
(displayln (Person-name (Book-author book)))
