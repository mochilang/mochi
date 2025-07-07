(define customers
  (list (list (cons 'id 1) (cons 'name "Alice"))
        (list (cons 'id 2) (cons 'name "Bob"))))

(define orders
  (list (list (cons 'id 100) (cons 'customerId 1) (cons 'total 250))
        (list (cons 'id 101) (cons 'customerId 3) (cons 'total 80))))

(define result '())
(for-each (lambda (o)
            (let ((c (let loop ((lst customers))
                        (if (null? lst) #f
                            (let ((c (car lst)))
                              (if (= (cdr (assoc 'id c)) (cdr (assoc 'customerId o)))
                                  c
                                  (loop (cdr lst))))))))
              (set! result
                    (cons (list (cons 'orderId (cdr (assoc 'id o)))
                                (cons 'customer c)
                                (cons 'total (cdr (assoc 'total o))))
                          result))))
          orders)

(display "--- Left Join ---")
(newline)
(for-each (lambda (e)
            (display "Order ")
            (display (cdr (assoc 'orderId e)))
            (display " customer ")
            (display (cdr (assoc 'customer e)))
            (display " total ")
            (display (cdr (assoc 'total e)))
            (newline))
          result)
