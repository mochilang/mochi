;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define matrix
  (list
    (list 1 2)
    (list 3 4)))
(list-set!
  (list-ref matrix 1) 0 5)
(display
  (cond
    ((string?
        (list-ref matrix 1))
      (string-ref
        (list-ref matrix 1) 0))
    ((hash-table?
        (list-ref matrix 1))
      (hash-table-ref
        (list-ref matrix 1) 0))
    (else
      (list-ref
        (list-ref matrix 1) 0))))
(newline)
