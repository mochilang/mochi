;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define data
  (alist->hash-table
    (list
      (cons "outer"
        (alist->hash-table
          (list
            (cons "inner" 1)))))))
(hash-table-set!
  (hash-table-ref data "outer") "inner" 2)
(display
  (cond
    ((string?
        (hash-table-ref data "outer"))
      (string-ref
        (hash-table-ref data "outer") "inner"))
    ((hash-table?
        (hash-table-ref data "outer"))
      (hash-table-ref
        (hash-table-ref data "outer") "inner"))
    (else
      (list-ref
        (hash-table-ref data "outer") "inner"))))
(newline)
