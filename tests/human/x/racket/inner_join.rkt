#lang racket
(define customers
  (list (hash 'id 1 'name "Alice")
        (hash 'id 2 'name "Bob")
        (hash 'id 3 'name "Charlie")))
(define orders
  (list (hash 'id 100 'customerId 1 'total 250)
        (hash 'id 101 'customerId 2 'total 125)
        (hash 'id 102 'customerId 1 'total 300)
        (hash 'id 103 'customerId 4 'total 80)))

(define result
  (for*/list ([o orders]
              [c customers #:when (= (hash-ref o 'customerId) (hash-ref c 'id))])
    (hash 'orderId (hash-ref o 'id)
          'customerName (hash-ref c 'name)
          'total (hash-ref o 'total))))

(displayln "--- Orders with customer info ---")
(for ([e result])
  (displayln (format "Order ~a by ~a - $ ~a"
                     (hash-ref e 'orderId)
                     (hash-ref e 'customerName)
                     (hash-ref e 'total))))
