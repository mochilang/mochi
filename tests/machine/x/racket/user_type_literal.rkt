#lang racket
(struct Person (name age) #:transparent)
(struct Book (title author) #:transparent)
(define book (Book "Go" (Person "Bob" 42)))
(displayln (Person-name (Book-author book)))
