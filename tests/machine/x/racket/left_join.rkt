#lang racket
(require racket/list)
(define customers (list (hash 'id 1 'name "Alice") (hash 'id 2 'name "Bob")))
(define orders (list (hash 'id 100 'customerId 1 'total 250) (hash 'id 101 'customerId 3 'total 80)))
(define result (for*/list ([o orders]) (let ((c (findf (lambda (c) (equal? (hash-ref o 'customerId) (hash-ref c 'id))) customers))) (hash 'orderId (hash-ref o 'id) 'customer c 'total (hash-ref o 'total)))))
(displayln "--- Left Join ---")
(for ([entry (if (hash? result) (hash-keys result) result)])
(displayln (string-join (map ~a (list "Order" (hash-ref entry 'orderId) "customer" (hash-ref entry 'customer) "total" (hash-ref entry 'total))) " "))
)
