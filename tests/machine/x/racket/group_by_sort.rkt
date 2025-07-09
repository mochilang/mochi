#lang racket
(define items (list (hash 'cat "a" 'val 3) (hash 'cat "a" 'val 1) (hash 'cat "b" 'val 5) (hash 'cat "b" 'val 2)))
(define grouped (for*/list ([i items]) (hash 'cat (hash-ref g 'key) 'total (apply + (for*/list ([x g]) (hash-ref x 'val))))))
(displayln grouped)
