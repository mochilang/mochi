(define (any pred lst) (cond ((null? lst) #f) ((pred (car lst)) #t) (else (any pred (cdr lst)))))
(define customers
  (list (list (cons 'id 1) (cons 'name "Alice"))
        (list (cons 'id 2) (cons 'name "Bob"))
        (list (cons 'id 3) (cons 'name "Charlie"))
        (list (cons 'id 4) (cons 'name "Diana"))))

(define orders
  (list (list (cons 'id 100) (cons 'customerId 1) (cons 'total 250))
        (list (cons 'id 101) (cons 'customerId 2) (cons 'total 125))
        (list (cons 'id 102) (cons 'customerId 1) (cons 'total 300))
        (list (cons 'id 103) (cons 'customerId 5) (cons 'total 80))))

(define result '())
(for-each (lambda (o)
            (let ((c (let loop ((lst customers))
                        (if (null? lst) #f
                            (let ((cu (car lst)))
                              (if (= (cdr (assoc 'id cu)) (cdr (assoc 'customerId o)))
                                  cu
                                  (loop (cdr lst))))))))
              (set! result (cons (list (cons 'order o) (cons 'customer c)) result))))
          orders)

(for-each (lambda (c)
            (unless (any (lambda (o) (= (cdr (assoc 'customerId (cdr (assoc 'order o)))) (cdr (assoc 'id c)))) result)
              (set! result (cons (list (cons 'order #f) (cons 'customer c)) result))))
          customers)

(display "--- Outer Join using syntax ---")
(newline)
(for-each
 (lambda (row)
   (let ((o (cdr (assoc 'order row)))
         (c (cdr (assoc 'customer row))))
     (if o
         (if c
             (begin (display "Order ")
                    (display (cdr (assoc 'id o)))
                    (display " by ")
                    (display (cdr (assoc 'name c)))
                    (display " - $")
                    (display (cdr (assoc 'total o)))
                    (newline))
             (begin (display "Order ")
                    (display (cdr (assoc 'id o)))
                    (display " by Unknown - $")
                    (display (cdr (assoc 'total o)))
                    (newline)))
         (begin (display "Customer ")
                (display (cdr (assoc 'name c)))
                (display " has no orders")
                (newline))))
 result)
