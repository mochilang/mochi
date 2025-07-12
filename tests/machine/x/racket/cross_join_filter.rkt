#lang racket
(define nums '(1 2 3))
(define letters '("A" "B"))
(define pairs (for*/list ([n nums] [l letters] #:when (and (equal? (remainder n 2) 0))) (hash 'n n 'l l)))
(displayln "--- Even pairs ---")
(for ([p (if (hash? pairs) (hash-keys pairs) pairs)])
(displayln (string-join (map ~a (list (hash-ref p 'n) (hash-ref p 'l))) " "))
)
