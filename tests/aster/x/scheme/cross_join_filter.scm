;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define nums
  (list 1 2 3))
(define letters
  (list "A" "B"))
(define pairs
  (let
    ((res2
        (list)))
    (begin
      (for-each
        (lambda
          (n)
          (for-each
            (lambda
              (l)
              (if
                (=
                  (modulo n 2) 0)
                (set! res2
                  (append res2
                    (list
                      (alist->hash-table
                        (list
                          (cons "n" n)
                          (cons "l" l))))))
                (quote nil))) letters)) nums) res2)))
(display "--- Even pairs ---")
(newline)
(for-each
  (lambda
    (p)
    (begin
      (display
        (hash-table-ref p "n"))
      (display " ")
      (display
        (hash-table-ref p "l"))
      (newline))) pairs)
