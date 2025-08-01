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
        (cons "total" 300)))))
(define result
  (let
    ((res43
        (list)))
    (begin
      (for-each
        (lambda
          (o)
          (let
            ((matched44 "false"))
            (begin
              (for-each
                (lambda
                  (c)
                  (if
                    (=
                      (hash-table-ref o "customerId")
                      (hash-table-ref c "id"))
                    (begin
                      (set! matched44 "true")
                      (set! res43
                        (append res43
                          (list
                            (alist->hash-table
                              (list
                                (cons "customerName"
                                  (hash-table-ref c "name"))
                                (cons "order" o)))))))
                    (quote nil))) customers)
              (if
                (not matched44)
                (let
                  ((c
                      (quote nil)))
                  (set! res43
                    (append res43
                      (list
                        (alist->hash-table
                          (list
                            (cons "customerName"
                              (hash-table-ref c "name"))
                            (cons "order" o)))))))
                (quote nil))))) orders) res43)))
(display "--- Right Join using syntax ---")
(newline)
(for-each
  (lambda
    (entry)
    (begin
      (if
        (hash-table-ref entry "order")
        (begin
          (display "Customer")
          (display " ")
          (display
            (hash-table-ref entry "customerName"))
          (display " ")
          (display "has order")
          (display " ")
          (display
            (hash-table-ref
              (hash-table-ref entry "order") "id"))
          (display " ")
          (display "- $")
          (display " ")
          (display
            (hash-table-ref
              (hash-table-ref entry "order") "total"))
          (newline))
        (begin
          (display "Customer")
          (display " ")
          (display
            (hash-table-ref entry "customerName"))
          (display " ")
          (display "has no orders")
          (newline))))) result)
