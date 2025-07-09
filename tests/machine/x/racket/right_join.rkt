#lang racket
(require racket/list)
(define customers (list (hash 'id 1 'name "Alice") (hash 'id 2 'name "Bob") (hash 'id 3 'name "Charlie") (hash 'id 4 'name "Diana")))
(define orders (list (hash 'id 100 'customerId 1 'total 250) (hash 'id 101 'customerId 2 'total 125) (hash 'id 102 'customerId 1 'total 300)))
(define result (for*/list ([o orders]) (let ((c (findf (lambda (c) (equal? (hash-ref o 'customerId) (hash-ref c 'id))) customers))) (hash 'customerName (hash-ref c 'name) 'order o))))
(displayln "--- Right Join using syntax ---")
(for ([entry (if (hash? result) (hash-keys result) result)])
(if (hash-ref entry 'order)
  (begin
(displayln (string-join (map ~a (list "Customer" (hash-ref entry 'customerName) "has order" (hash-ref (hash-ref entry 'order) 'id) "- $" (hash-ref (hash-ref entry 'order) 'total))) " "))
  )
  (begin
(displayln (string-join (map ~a (list "Customer" (hash-ref entry 'customerName) "has no orders")) " "))
  )
)
)
