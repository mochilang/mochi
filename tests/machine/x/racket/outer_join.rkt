#lang racket
(require racket/list)
(define customers (list (hash 'id 1 'name "Alice") (hash 'id 2 'name "Bob") (hash 'id 3 'name "Charlie") (hash 'id 4 'name "Diana")))
(define orders (list (hash 'id 100 'customerId 1 'total 250) (hash 'id 101 'customerId 2 'total 125) (hash 'id 102 'customerId 1 'total 300) (hash 'id 103 'customerId 5 'total 80)))
(define result (append (for/list ([o orders]) (let ([c (findf (lambda (c) (equal? (hash-ref o 'customerId) (hash-ref c 'id))) customers)]) (hash 'order o 'customer c))) (for/list ([c customers] #:unless (for/or ([o orders]) (equal? (hash-ref o 'customerId) (hash-ref c 'id)))) (let ([o #f]) (hash 'order o 'customer c)))))
(displayln "--- Outer Join using syntax ---")
(for ([row (if (hash? result) (hash-keys result) result)])
(if (hash-ref row 'order)
  (begin
(if (hash-ref row 'customer)
  (begin
(displayln (string-join (map ~a (list "Order" (hash-ref (hash-ref row 'order) 'id) "by" (hash-ref (hash-ref row 'customer) 'name) "- $" (hash-ref (hash-ref row 'order) 'total))) " "))
  )
  (begin
(displayln (string-join (map ~a (list "Order" (hash-ref (hash-ref row 'order) 'id) "by" "Unknown" "- $" (hash-ref (hash-ref row 'order) 'total))) " "))
  )
)
  )
  (begin
(displayln (string-join (map ~a (list "Customer" (hash-ref (hash-ref row 'customer) 'name) "has no orders")) " "))
  )
)
)
