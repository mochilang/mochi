(define customers
  (list (list (cons 'id 1) (cons 'name "Alice"))
        (list (cons 'id 2) (cons 'name "Bob"))))

(define orders
  (list (list (cons 'id 100) (cons 'customerId 1))
        (list (cons 'id 101) (cons 'customerId 2))))

(define items
  (list (list (cons 'orderId 100) (cons 'sku "a"))
        (list (cons 'orderId 101) (cons 'sku "b"))))

(define result '())
(for-each (lambda (o)
            (for-each (lambda (c)
                        (when (= (cdr (assoc 'id c)) (cdr (assoc 'customerId o)))
                          (for-each (lambda (i)
                                      (when (= (cdr (assoc 'orderId i)) (cdr (assoc 'id o)))
                                        (set! result
                                              (cons (list (cons 'name (cdr (assoc 'name c)))
                                                          (cons 'sku (cdr (assoc 'sku i))))
                                                    result))))
                                    items)))
                      customers))
          orders)

(display "--- Multi Join ---")
(newline)
(for-each (lambda (r)
            (display (cdr (assoc 'name r)))
            (display " bought item ")
            (display (cdr (assoc 'sku r)))
            (newline))
          result)
