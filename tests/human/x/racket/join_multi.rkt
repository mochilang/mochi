#lang racket
(define customers (list (hash 'id 1 'name "Alice")
                        (hash 'id 2 'name "Bob")))
(define orders (list (hash 'id 100 'customerId 1)
                     (hash 'id 101 'customerId 2)))
(define items (list (hash 'orderId 100 'sku "a")
                    (hash 'orderId 101 'sku "b")))

(define result
  (for*/list ([o orders]
              [c customers #:when (= (hash-ref o 'customerId) (hash-ref c 'id))]
              [i items #:when (= (hash-ref o 'id) (hash-ref i 'orderId))])
    (hash 'name (hash-ref c 'name) 'sku (hash-ref i 'sku))))

(displayln "--- Multi Join ---")
(for ([r result])
  (displayln (format "~a bought item ~a" (hash-ref r 'name) (hash-ref r 'sku))))
