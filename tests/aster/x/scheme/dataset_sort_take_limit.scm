;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define products
  (list
    (alist->hash-table
      (list
        (cons "name" "Laptop")
        (cons "price" 1500)))
    (alist->hash-table
      (list
        (cons "name" "Smartphone")
        (cons "price" 900)))
    (alist->hash-table
      (list
        (cons "name" "Tablet")
        (cons "price" 600)))
    (alist->hash-table
      (list
        (cons "name" "Monitor")
        (cons "price" 300)))
    (alist->hash-table
      (list
        (cons "name" "Keyboard")
        (cons "price" 100)))
    (alist->hash-table
      (list
        (cons "name" "Mouse")
        (cons "price" 50)))
    (alist->hash-table
      (list
        (cons "name" "Headphones")
        (cons "price" 200)))))
(define expensive
  (let
    ((res4
        (list)))
    (begin
      (for-each
        (lambda
          (p)
          (set! res4
            (append res4
              (list p)))) products) res4)))
(display "--- Top products (excluding most expensive)
   ---")
(newline)
(for-each
  (lambda
    (item)
    (begin
      (display
        (hash-table-ref item "name"))
      (display " ")
      (display "costs $")
      (display " ")
      (display
        (hash-table-ref item "price"))
      (newline))) expensive)
