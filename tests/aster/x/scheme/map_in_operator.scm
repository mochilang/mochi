;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define m
  (alist->hash-table
    (list
      (cons 1 "a")
      (cons 2 "b"))))
(display
  (if
    (cond
      ((string? m)
        (if
          (string-contains m 1) "true" "false"))
      ((hash-table? m)
        (if
          (hash-table-exists? m 1) "true" "false"))
      (else
        (if
          (member 1 m) "true" "false"))) 1 0))
(newline)
(display
  (if
    (cond
      ((string? m)
        (if
          (string-contains m 3) "true" "false"))
      ((hash-table? m)
        (if
          (hash-table-exists? m 3) "true" "false"))
      (else
        (if
          (member 3 m) "true" "false"))) 1 0))
(newline)
