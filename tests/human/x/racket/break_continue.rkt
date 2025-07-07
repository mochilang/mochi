#lang racket
(define numbers '(1 2 3 4 5 6 7 8 9))

(define (loop lst)
  (cond
    [(empty? lst) (void)]
    [else
     (define n (car lst))
     (cond
       [(even? n) (loop (cdr lst))] ; continue
       [(> n 7) (void)]            ; break
       [else
        (displayln (string-append "odd number:" (number->string n)))
        (loop (cdr lst))])]))

(loop numbers)
