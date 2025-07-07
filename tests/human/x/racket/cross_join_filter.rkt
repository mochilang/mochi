#lang racket
(define nums '(1 2 3))
(define letters '("A" "B"))
(define pairs
  (for*/list ([n nums]
              [l letters]
              #:when (even? n))
    (hash 'n n 'l l)) )
(displayln "--- Even pairs ---")
(for ([p pairs])
  (displayln (string-append (number->string (hash-ref p 'n)) " " (hash-ref p 'l))))
