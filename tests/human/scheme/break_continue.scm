(define numbers '(1 2 3 4 5 6 7 8 9))
(define (process lst)
  (cond
    ((null? lst) #t)
    (else
     (let ((n (car lst)))
       (cond
         ((even? n) (process (cdr lst)))
         ((> n 7) #t)
         (else
          (display "odd number:")
          (display " ")
          (display n)
          (newline)
          (process (cdr lst)))))))
(process numbers)
