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
  (hash-table-ref m 1))
(newline)
