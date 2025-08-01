;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define people
  (list
    (alist->hash-table
      (list
        (cons "name" "Alice")
        (cons "age" 30)))
    (alist->hash-table
      (list
        (cons "name" "Bob")
        (cons "age" 15)))
    (alist->hash-table
      (list
        (cons "name" "Charlie")
        (cons "age" 65)))
    (alist->hash-table
      (list
        (cons "name" "Diana")
        (cons "age" 45)))))
(define adults
  (let
    ((res5
        (list)))
    (begin
      (for-each
        (lambda
          (person)
          (if
            (>=
              (hash-table-ref person "age") 18)
            (set! res5
              (append res5
                (list
                  (alist->hash-table
                    (list
                      (cons "name"
                        (hash-table-ref person "name"))
                      (cons "age"
                        (hash-table-ref person "age"))
                      (cons "is_senior"
                        (>=
                          (hash-table-ref person "age") 60)))))))
            (quote nil))) people) res5)))
(display "--- Adults ---")
(newline)
(for-each
  (lambda
    (person)
    (begin
      (display
        (hash-table-ref person "name"))
      (display " ")
      (display "is")
      (display " ")
      (display
        (hash-table-ref person "age"))
      (display " ")
      (display
        (if
          (hash-table-ref person "is_senior") " (senior)
          " ""))
      (newline))) adults)
