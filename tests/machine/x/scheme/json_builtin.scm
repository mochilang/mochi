(define (_json v)
  (write v)
  (newline))

(define m (list (cons 'a 1) (cons 'b 2)))
(_json m)
