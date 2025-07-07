(define customers
  (list (list (cons 'id 1) (cons 'name "Alice"))
        (list (cons 'id 2) (cons 'name "Bob"))))

(define orders
  (list (list (cons 'id 100) (cons 'customerId 1))
        (list (cons 'id 101) (cons 'customerId 2))))

(define items
  (list (list (cons 'orderId 100) (cons 'sku "a"))))

(define result '())
(for-each (lambda (o)
            (let* ((c (let loop ((lst customers))
                         (if (null? lst) #f
                             (let ((c (car lst)))
                               (if (= (cdr (assoc 'id c)) (cdr (assoc 'customerId o)))
                                   c
                                   (loop (cdr lst)))))) )
                   (i (let loop ((lst items))
                        (if (null? lst) #f
                            (let ((it (car lst)))
                              (if (= (cdr (assoc 'orderId it)) (cdr (assoc 'id o)))
                                  it
                                  (loop (cdr lst))))))))
              (set! result
                    (cons (list (cons 'orderId (cdr (assoc 'id o)))
                                (cons 'name (cdr (assoc 'name c)))
                                (cons 'item i))
                          result))))
          orders)

(display "--- Left Join Multi ---")
(newline)
(for-each (lambda (r)
            (display (cdr (assoc 'orderId r)))
            (display " ")
            (display (cdr (assoc 'name r)))
            (display " ")
            (display (cdr (assoc 'item r)))
            (newline))
          result)
