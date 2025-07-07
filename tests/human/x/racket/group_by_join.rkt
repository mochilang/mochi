#lang racket
(define customers
  (list (hash 'id 1 'name "Alice")
        (hash 'id 2 'name "Bob")))
(define orders
  (list (hash 'id 100 'customerId 1)
        (hash 'id 101 'customerId 1)
        (hash 'id 102 'customerId 2)))

;; join orders with customers and count per customer name
(define stats (make-hash))
(for ([o orders])
  (define c (findf (lambda (x) (= (hash-ref x 'id) (hash-ref o 'customerId))) customers))
  (define name (hash-ref c 'name))
  (define count (hash-ref stats name 0))
  (hash-set! stats name (add1 count)))

(displayln "--- Orders per customer ---")
(for ([name (hash-keys stats)])
  (displayln (format "~a orders: ~a" name (hash-ref stats name))))
