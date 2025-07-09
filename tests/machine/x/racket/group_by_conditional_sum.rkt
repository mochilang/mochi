#lang racket
(define items (list (hash 'cat "a" 'val 10 'flag #t) (hash 'cat "a" 'val 5 'flag #f) (hash 'cat "b" 'val 20 'flag #t)))
(define result (for*/list ([i items]) (hash 'cat (hash-ref g 'key) 'share (/ (apply + (for*/list ([x g]) (if (hash-ref x 'flag) (hash-ref x 'val) 0))) (apply + (for*/list ([x g]) (hash-ref x 'val)))))))
(displayln result)
