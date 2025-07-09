#lang racket
(define customers (list (hash 'id 1 'name "Alice") (hash 'id 2 'name "Bob") (hash 'id 3 'name "Charlie")))
(define orders (list (hash 'id 100 'customerId 1 'total 250) (hash 'id 101 'customerId 2 'total 125) (hash 'id 102 'customerId 1 'total 300)))
(define result (for*/list ([o orders] [c customers]) (hash 'orderId (hash-ref o 'id) 'orderCustomerId (hash-ref o 'customerId) 'pairedCustomerName (hash-ref c 'name) 'orderTotal (hash-ref o 'total))))
(displayln "--- Cross Join: All order-customer pairs ---")
(for ([entry (if (hash? result) (hash-keys result) result)])
(displayln (string-join (map ~a (list "Order" (hash-ref entry 'orderId) "(customerId:" (hash-ref entry 'orderCustomerId) ", total: $" (hash-ref entry 'orderTotal) ") paired with" (hash-ref entry 'pairedCustomerName))) " "))
)
