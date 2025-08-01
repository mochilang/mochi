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
        (cons "sku" "a")))
    (alist->hash-table
      (list
        (cons "orderId" 101)
        (cons "sku" "b")))))
(define result
  (let
    ((res37
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
                    (set! res37
                      (append res37
                        (list
                          (alist->hash-table
                            (list
                              (cons "name"
                                (hash-table-ref c "name"))
                              (cons "sku"
                                (hash-table-ref i "sku")))))))
                    (quote nil))) items)) customers)) orders) res37)))
(display "--- Multi Join ---")
(newline)
(for-each
  (lambda
    (r)
    (begin
      (display
        (hash-table-ref r "name"))
      (display " ")
      (display "bought item")
      (display " ")
      (display
        (hash-table-ref r "sku"))
      (newline))) result)
