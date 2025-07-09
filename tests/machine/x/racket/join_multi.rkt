#lang racket
(define customers (list (hash 'id 1 'name "Alice") (hash 'id 2 'name "Bob")))
(define orders (list (hash 'id 100 'customerId 1) (hash 'id 101 'customerId 2)))
(define items (list (hash 'orderId 100 'sku "a") (hash 'orderId 101 'sku "b")))
(define result (for*/list ([o orders]) (hash 'name (hash-ref c 'name) 'sku (hash-ref i 'sku))))
(displayln "--- Multi Join ---")
(for ([r (if (hash? result) (hash-keys result) result)])
(displayln (string-join (map ~a (list (hash-ref r 'name) "bought item" (hash-ref r 'sku))) " "))
)
