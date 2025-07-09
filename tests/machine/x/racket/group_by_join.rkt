#lang racket
(define customers (list (hash 'id 1 'name "Alice") (hash 'id 2 'name "Bob")))
(define orders (list (hash 'id 100 'customerId 1) (hash 'id 101 'customerId 1) (hash 'id 102 'customerId 2)))
(define stats (for*/list ([o orders]) (hash 'name (hash-ref g 'key) 'count (length g))))
(displayln "--- Orders per customer ---")
(for ([s (if (hash? stats) (hash-keys stats) stats)])
(displayln (string-join (map ~a (list (hash-ref s 'name) "orders:" (hash-ref s 'count))) " "))
)
