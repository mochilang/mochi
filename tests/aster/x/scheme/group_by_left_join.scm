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
        (cons "name" "Charlie")))))
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
    ((groups24
        (make-hash-table))
      (res27
        (list)))
    (begin
      (for-each
        (lambda
          (c)
          (for-each
            (lambda
              (o)
              (if
                (=
                  (hash-table-ref o "customerId")
                  (hash-table-ref c "id"))
                (let*
                  ((k26
                      (hash-table-ref c "name"))
                    (g25
                      (hash-table-ref/default groups24 k26 #f)))
                  (begin
                    (if
                      (not g25)
                      (begin
                        (set! g25
                          (alist->hash-table
                            (list
                              (cons "key" k26)
                              (cons "items"
                                (list)))))
                        (hash-table-set! groups24 k26 g25))
                      (quote nil))
                    (hash-table-set! g25 "items"
                      (append
                        (hash-table-ref g25 "items")
                        (list
                          (alist->hash-table
                            (list
                              (cons "c" c)
                              (cons "o" o))))))))
                (quote nil))) orders)) customers)
      (for-each
        (lambda
          (g)
          (set! res27
            (append res27
              (list
                (alist->hash-table
                  (list
                    (cons "name"
                      (hash-table-ref g "key"))
                    (cons "count"
                      (length
                        (let
                          ((res23
                              (list)))
                          (begin
                            (for-each
                              (lambda
                                (r)
                                (if
                                  (hash-table-ref r "o")
                                  (set! res23
                                    (append res23
                                      (list r)))
                                  (quote nil)))
                              (hash-table-ref g "items")) res23))))))))))
        (hash-table-values groups24)) res27)))
(display "--- Group Left Join ---")
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
