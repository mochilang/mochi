;; Generated on 2025-07-21 17:26 +0700
(import
  (srfi 1)
  (srfi 69)
  (chibi string))
(define data
  (list
    (alist->hash-table
      (list
        (cons "a" 1)
        (cons "b" 2)))
    (alist->hash-table
      (list
        (cons "a" 1)
        (cons "b" 1)))
    (alist->hash-table
      (list
        (cons "a" 0)
        (cons "b" 5)))))
(define sorted
  (let
    ((res40
        (list)))
    (begin
      (for-each
        (lambda
          (x)
          (set! res40
            (append res40
              (list x)))) data) res40)))
(display sorted)
(newline)
