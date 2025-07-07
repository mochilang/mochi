#lang racket
(define customers (list (hash 'id 1 'name "Alice")
                        (hash 'id 2 'name "Bob")))
(define orders (list (hash 'id 100 'customerId 1 'total 250)
                     (hash 'id 101 'customerId 3 'total 80)))

(define result
  (for/list ([o orders])
    (define c (findf (lambda (x) (= (hash-ref o 'customerId) (hash-ref x 'id))) customers))
    (hash 'orderId (hash-ref o 'id)
          'customer c
          'total (hash-ref o 'total))))

(displayln "--- Left Join ---")
(for ([e result])
  (displayln (format "Order ~a customer ~a total ~a"
                     (hash-ref e 'orderId)
                     (hash-ref e 'customer)
                     (hash-ref e 'total))))
