#lang racket
(define customers
  (list (hash 'id 1 'name "Alice")
        (hash 'id 2 'name "Bob")
        (hash 'id 3 'name "Charlie")
        (hash 'id 4 'name "Diana")))
(define orders
  (list (hash 'id 100 'customerId 1 'total 250)
        (hash 'id 101 'customerId 2 'total 125)
        (hash 'id 102 'customerId 1 'total 300)))

(define result '())
(for ([c customers])
  (define ords (filter (lambda (o) (= (hash-ref o 'customerId) (hash-ref c 'id))) orders))
  (if (null? ords)
      (set! result (append result (list (hash 'customerName (hash-ref c 'name) 'order #f))))
      (for ([o ords])
        (set! result (append result (list (hash 'customerName (hash-ref c 'name) 'order o)))))))

(displayln "--- Right Join using syntax ---")
(for ([e result])
  (if (hash-ref e 'order)
      (displayln (format "Customer ~a has order ~a - $ ~a"
                         (hash-ref e 'customerName)
                         (hash-ref (hash-ref e 'order) 'id)
                         (hash-ref (hash-ref e 'order) 'total)))
      (displayln (format "Customer ~a has no orders" (hash-ref e 'customerName)))))
