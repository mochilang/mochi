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
        (cons "customerId" 1)))
    (alist->hash-table
      (list
        (cons "id" 101)
        (cons "customerId" 2)))))
(define items
  (list
    (alist->hash-table
      (list
        (cons "orderId" 100)
        (cons "sku" "a")))))
(define result
  (let
    ((res39
        (list)))
    (begin
      (for-each
        (lambda
          (o)
          (for-each
            (lambda
              (c)
              (for-each
                (lambda
                  (i)
                  (if
                    (and
                      (=
                        (hash-table-ref o "customerId")
                        (hash-table-ref c "id"))
                      (=
                        (hash-table-ref o "id")
                        (hash-table-ref i "orderId")))
                    (set! res39
                      (append res39
                        (list
                          (alist->hash-table
                            (list
                              (cons "orderId"
                                (hash-table-ref o "id"))
                              (cons "name"
                                (hash-table-ref c "name"))
                              (cons "item" i))))))
                    (quote nil))) items)) customers)) orders) res39)))
(display "--- Left Join Multi ---")
(newline)
(for-each
  (lambda
    (r)
    (begin
      (display
        (hash-table-ref r "orderId"))
      (display " ")
      (display
        (hash-table-ref r "name"))
      (display " ")
      (display
        (hash-table-ref r "item"))
      (newline))) result)
