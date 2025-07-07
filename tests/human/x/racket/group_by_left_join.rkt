#lang racket
(define customers
  (list (hash 'id 1 'name "Alice")
        (hash 'id 2 'name "Bob")
        (hash 'id 3 'name "Charlie"))) ; no orders
(define orders
  (list (hash 'id 100 'customerId 1)
        (hash 'id 101 'customerId 1)
        (hash 'id 102 'customerId 2)))

(define groups (make-hash))
(for ([c customers])
  (hash-set! groups (hash-ref c 'name) '()))
(for ([o orders])
  (define c (findf (lambda (x) (= (hash-ref x 'id) (hash-ref o 'customerId))) customers))
  (define name (hash-ref c 'name))
  (hash-set! groups name (cons o (hash-ref groups name '()))))

(displayln "--- Group Left Join ---")
(for ([name (hash-keys groups)])
  (define cnt (length (hash-ref groups name)))
  (displayln (format "~a orders: ~a" name cnt)))
