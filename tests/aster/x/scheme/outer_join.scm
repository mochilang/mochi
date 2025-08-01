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
        (cons "name" "Bob")))
    (alist->hash-table
      (list
        (cons "id" 3)
        (cons "name" "Charlie")))
    (alist->hash-table
      (list
        (cons "id" 4)
        (cons "name" "Diana")))))
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
        (cons "customerId" 2)
        (cons "total" 125)))
    (alist->hash-table
      (list
        (cons "id" 102)
        (cons "customerId" 1)
        (cons "total" 300)))
    (alist->hash-table
      (list
        (cons "id" 103)
        (cons "customerId" 5)
        (cons "total" 80)))))
(define result
  (let
    ((res41
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
                (set! res41
                  (append res41
                    (list
                      (alist->hash-table
                        (list
                          (cons "order" o)
                          (cons "customer" c))))))
                (quote nil))) customers)) orders) res41)))
(display "--- Outer Join using syntax ---")
(newline)
(for-each
  (lambda
    (row)
    (begin
      (if
        (hash-table-ref row "order")
        (begin
          (if
            (hash-table-ref row "customer")
            (begin
              (display "Order")
              (display " ")
              (display
                (hash-table-ref
                  (hash-table-ref row "order") "id"))
              (display " ")
              (display "by")
              (display " ")
              (display
                (hash-table-ref
                  (hash-table-ref row "customer") "name"))
              (display " ")
              (display "- $")
              (display " ")
              (display
                (hash-table-ref
                  (hash-table-ref row "order") "total"))
              (newline))
            (begin
              (display "Order")
              (display " ")
              (display
                (hash-table-ref
                  (hash-table-ref row "order") "id"))
              (display " ")
              (display "by")
              (display " ")
              (display "Unknown")
              (display " ")
              (display "- $")
              (display " ")
              (display
                (hash-table-ref
                  (hash-table-ref row "order") "total"))
              (newline))))
        (begin
          (display "Customer")
          (display " ")
          (display
            (hash-table-ref
              (hash-table-ref row "customer") "name"))
          (display " ")
          (display "has no orders")
          (newline))))) result)
