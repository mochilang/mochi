;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define scores
  (alist->hash-table
    (list
      (cons "alice" 1))))
(hash-table-set! scores "bob" 2)
(display
  (hash-table-ref scores "bob"))
(newline)
