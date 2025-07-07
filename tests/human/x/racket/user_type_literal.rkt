#lang racket
(struct person (name age))
(struct book (title author))
(define book-instance (book "Go" (person "Bob" 42)))
(displayln (person-name (book-author book-instance)))
