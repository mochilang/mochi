#lang racket
(define customers (list (hash 'id 1 'name "Alice") (hash 'id 2 'name "Bob") (hash 'id 3 'name "Charlie")))
(define orders (list (hash 'id 100 'customerId 1) (hash 'id 101 'customerId 1) (hash 'id 102 'customerId 2)))
(define stats (for*/list ([c customers]) (hash 'name (hash-ref g 'key) 'count (length (for*/list ([r g] #:when (hash-ref r 'o)) r)))))
(displayln "--- Group Left Join ---")
(for ([s (if (hash? stats) (hash-keys stats) stats)])
(displayln (string-join (map ~a (list (hash-ref s 'name) "orders:" (hash-ref s 'count))) " "))
)
