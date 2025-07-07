(define customers
  (list
    (list (cons 'id 1) (cons 'name "Alice"))
    (list (cons 'id 2) (cons 'name "Bob"))
    (list (cons 'id 3) (cons 'name "Charlie"))))

(define orders
  (list
    (list (cons 'id 100) (cons 'customerId 1))
    (list (cons 'id 101) (cons 'customerId 1))
    (list (cons 'id 102) (cons 'customerId 2))))

(define stats '())

(for-each
 (lambda (c)
   (let ((count 0))
     (for-each (lambda (o)
                 (when (= (cdr (assoc 'customerId o))
                          (cdr (assoc 'id c)))
                   (set! count (+ count 1))))
               orders)
     (set! stats
           (append stats
                   (list (list (cons 'name (cdr (assoc 'name c)))
                               (cons 'count count)))))))
 customers)

(display "--- Group Left Join ---")
(newline)
(for-each (lambda (s)
            (display (cdr (assoc 'name s)))
            (display " orders:")
            (display (cdr (assoc 'count s)))
            (newline))
          stats)
