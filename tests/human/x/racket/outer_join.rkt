#lang racket
(define customers
  (list (hash 'id 1 'name "Alice")
        (hash 'id 2 'name "Bob")
        (hash 'id 3 'name "Charlie")
        (hash 'id 4 'name "Diana"))) ; has no order

(define orders
  (list (hash 'id 100 'customerId 1 'total 250)
        (hash 'id 101 'customerId 2 'total 125)
        (hash 'id 102 'customerId 1 'total 300)
        (hash 'id 103 'customerId 5 'total 80))) ; unknown customer

(define result '())
(for ([o orders])
  (define c (findf (lambda (x) (= (hash-ref o 'customerId) (hash-ref x 'id))) customers))
  (set! result (append result (list (hash 'order o 'customer c)))) )
(for ([c customers])
  (unless (for/or ([o orders]) (= (hash-ref o 'customerId) (hash-ref c 'id)))
    (set! result (append result (list (hash 'order #f 'customer c))))) )

(displayln "--- Outer Join using syntax ---")
(for ([row result])
  (cond [(hash-ref row 'order)
         (if (hash-ref row 'customer)
             (displayln (format "Order ~a by ~a - $ ~a"
                                 (hash-ref (hash-ref row 'order) 'id)
                                 (hash-ref (hash-ref row 'customer) 'name)
                                 (hash-ref (hash-ref row 'order) 'total)))
             (displayln (format "Order ~a by Unknown - $ ~a"
                                 (hash-ref (hash-ref row 'order) 'id)
                                 (hash-ref (hash-ref row 'order) 'total))))]
        [else
         (displayln (format "Customer ~a has no orders"
                            (hash-ref (hash-ref row 'customer) 'name)))]))
