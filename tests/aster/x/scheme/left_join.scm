;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define customers
  (list
    (alist->hash-table
      (list
        (cons "id" 1)
        (cons "name" "Alice")))
    (alist->hash-table
      (list
        (cons "id" 2)
        (cons "name" "Bob")))))
(define orders
  (list
    (alist->hash-table
      (list
        (cons "id" 100)
        (cons "customerId" 1)
        (cons "total" 250)))
    (alist->hash-table
      (list
        (cons "id" 101)
        (cons "customerId" 3)
        (cons "total" 80)))))
(define result
  (let
    ((res38
        (list)))
    (begin
      (for-each
        (lambda
          (o)
          (for-each
            (lambda
              (c)
              (if
                (=
                  (hash-table-ref o "customerId")
                  (hash-table-ref c "id"))
                (set! res38
                  (append res38
                    (list
                      (alist->hash-table
                        (list
                          (cons "orderId"
                            (hash-table-ref o "id"))
                          (cons "customer" c)
                          (cons "total"
                            (hash-table-ref o "total")))))))
                (quote nil))) customers)) orders) res38)))
(display "--- Left Join ---")
(newline)
(for-each
  (lambda
    (entry)
    (begin
      (display "Order")
      (display " ")
      (display
        (hash-table-ref entry "orderId"))
      (display " ")
      (display "customer")
      (display " ")
      (display
        (hash-table-ref entry "customer"))
      (display " ")
      (display "total")
      (display " ")
      (display
        (hash-table-ref entry "total"))
      (newline))) result)
