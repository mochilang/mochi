;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define m
  (alist->hash-table
    (list
      (cons "a" 1)
      (cons "b" 2))))
(display
  (if
    (cond
      ((string? m)
        (if
          (string-contains m "a") "true" "false"))
      ((hash-table? m)
        (if
          (hash-table-exists? m "a") "true" "false"))
      (else
        (if
          (member "a" m) "true" "false"))) 1 0))
(newline)
(display
  (if
    (cond
      ((string? m)
        (if
          (string-contains m "c") "true" "false"))
      ((hash-table? m)
        (if
          (hash-table-exists? m "c") "true" "false"))
      (else
        (if
          (member "c" m) "true" "false"))) 1 0))
(newline)
