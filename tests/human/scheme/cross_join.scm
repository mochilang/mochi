(define customers
  (list (list (cons 'id 1) (cons 'name "Alice"))
        (list (cons 'id 2) (cons 'name "Bob"))
        (list (cons 'id 3) (cons 'name "Charlie"))))

(define orders
  (list (list (cons 'id 100) (cons 'customerId 1) (cons 'total 250))
        (list (cons 'id 101) (cons 'customerId 2) (cons 'total 125))
        (list (cons 'id 102) (cons 'customerId 1) (cons 'total 300))))

(display "--- Cross Join: All order-customer pairs ---")
(newline)
(for-each
 (lambda (o)
   (for-each
    (lambda (c)
      (display "Order ")
      (display (cdr (assoc 'id o)))
      (display " (customerId: ")
      (display (cdr (assoc 'customerId o)))
      (display ", total: $")
      (display (cdr (assoc 'total o)))
      (display ") paired with ")
      (display (cdr (assoc 'name c)))
      (newline))
    customers))
 orders)
