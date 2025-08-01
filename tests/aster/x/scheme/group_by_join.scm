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
        (cons "customerId" 1)))
    (alist->hash-table
      (list
        (cons "id" 102)
        (cons "customerId" 2)))))
(define stats
  (let
    ((groups19
        (make-hash-table))
      (res22
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
                (let*
                  ((k21
                      (hash-table-ref c "name"))
                    (g20
                      (hash-table-ref/default groups19 k21 #f)))
                  (begin
                    (if
                      (not g20)
                      (begin
                        (set! g20
                          (alist->hash-table
                            (list
                              (cons "key" k21)
                              (cons "items"
                                (list)))))
                        (hash-table-set! groups19 k21 g20))
                      (quote nil))
                    (hash-table-set! g20 "items"
                      (append
                        (hash-table-ref g20 "items")
                        (list
                          (alist->hash-table
                            (list
                              (cons "o" o)
                              (cons "c" c))))))))
                (quote nil))) customers)) orders)
      (for-each
        (lambda
          (g)
          (set! res22
            (append res22
              (list
                (alist->hash-table
                  (list
                    (cons "name"
                      (hash-table-ref g "key"))
                    (cons "count"
                      (length
                        (hash-table-ref g "items")))))))))
        (hash-table-values groups19)) res22)))
(display "--- Orders per customer ---")
(newline)
(for-each
  (lambda
    (s)
    (begin
      (display
        (hash-table-ref s "name"))
      (display " ")
      (display "orders:")
      (display " ")
      (display
        (hash-table-ref s "count"))
      (newline))) stats)
