(define customers
  (list (list (cons 'id 1) (cons 'name "Alice"))
        (list (cons 'id 2) (cons 'name "Bob"))
        (list (cons 'id 3) (cons 'name "Charlie"))))

(define orders
  (list (list (cons 'id 100) (cons 'customerId 1) (cons 'total 250))
        (list (cons 'id 101) (cons 'customerId 2) (cons 'total 125))
        (list (cons 'id 102) (cons 'customerId 1) (cons 'total 300))
        (list (cons 'id 103) (cons 'customerId 4) (cons 'total 80))))

(define result '())
(for-each (lambda (o)
            (for-each (lambda (c)
                        (when (= (cdr (assoc 'id c)) (cdr (assoc 'customerId o)))
                          (set! result
                                (cons (list (cons 'orderId (cdr (assoc 'id o)))
                                            (cons 'customerName (cdr (assoc 'name c)))
                                            (cons 'total (cdr (assoc 'total o))))
                                      result))))
                      customers))
          orders)

(display "--- Orders with customer info ---")
(newline)
(for-each (lambda (e)
            (display "Order ")
            (display (cdr (assoc 'orderId e)))
            (display " by ")
            (display (cdr (assoc 'customerName e)))
            (display " - $")
            (display (cdr (assoc 'total e)))
            (newline))
          result)
