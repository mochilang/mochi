;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define nums
  (list 1 2))
(define letters
  (list "A" "B"))
(define bools
  (list "true" "false"))
(define combos
  (let
    ((res3
        (list)))
    (begin
      (for-each
        (lambda
          (n)
          (for-each
            (lambda
              (l)
              (for-each
                (lambda
                  (b)
                  (set! res3
                    (append res3
                      (list
                        (alist->hash-table
                          (list
                            (cons "n" n)
                            (cons "l" l)
                            (cons "b" b))))))) bools)) letters)) nums) res3)))
(display "--- Cross Join of three lists ---")
(newline)
(for-each
  (lambda
    (c)
    (begin
      (display
        (hash-table-ref c "n"))
      (display " ")
      (display
        (hash-table-ref c "l"))
      (display " ")
      (display
        (hash-table-ref c "b"))
      (newline))) combos)
