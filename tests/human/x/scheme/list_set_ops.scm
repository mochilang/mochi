(define (remove-dups lst)
  (fold-right (lambda (x acc) (if (member x acc) acc (cons x acc))) '() lst))

(define (list-union a b)
  (remove-dups (append a b)))

(define (list-except a b)
  (filter (lambda (x) (not (member x b))) a))

(define (list-intersect a b)
  (filter (lambda (x) (member x b)) a))

(display (list-union '(1 2) '(2 3)))
(newline)
(display (list-except '(1 2 3) '(2)))
(newline)
(display (list-intersect '(1 2 3) '(2 4)))
(newline)
(display (length (append '(1 2) '(2 3))))
(newline)
