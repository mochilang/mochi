(define (print-list lst)
  (display (car lst))
  (for-each (lambda (x) (display " ") (display x)) (cdr lst))
  (newline))

(let ((a (list 1 2)))
  (print-list (append a (list 3))))
