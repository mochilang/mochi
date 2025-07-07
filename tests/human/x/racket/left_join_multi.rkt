#lang racket
(define customers (list (hash 'id 1 'name "Alice")
                        (hash 'id 2 'name "Bob")))
(define orders (list (hash 'id 100 'customerId 1)
                     (hash 'id 101 'customerId 2)))
(define items (list (hash 'orderId 100 'sku "a")))

(define result
  (for*/list ([o orders]
              [c customers #:when (= (hash-ref o 'customerId) (hash-ref c 'id))])
    (define it (findf (lambda (x) (= (hash-ref x 'orderId) (hash-ref o 'id))) items))
    (hash 'orderId (hash-ref o 'id) 'name (hash-ref c 'name) 'item it)))

(displayln "--- Left Join Multi ---")
(for ([r result])
  (displayln (format "~a ~a ~a" (hash-ref r 'orderId) (hash-ref r 'name) (hash-ref r 'item))))
