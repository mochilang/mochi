#lang racket
(define customers (list (hash 'id 1 'name "Alice") (hash 'id 2 'name "Bob")))
(define orders (list (hash 'id 100 'customerId 1) (hash 'id 101 'customerId 2)))
(define items (list (hash 'orderId 100 'sku "a")))
(define result (for*/list ([o orders]) (hash 'orderId (hash-ref o 'id) 'name (hash-ref c 'name) 'item i)))
(displayln "--- Left Join Multi ---")
(for ([r (if (hash? result) (hash-keys result) result)])
(displayln (string-join (map ~a (list (hash-ref r 'orderId) (hash-ref r 'name) (hash-ref r 'item))) " "))
)
