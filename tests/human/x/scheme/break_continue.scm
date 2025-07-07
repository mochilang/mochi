(define numbers '(1 2 3 4 5 6 7 8 9))
(let loop ((lst numbers))
  (when (pair? lst)
    (let ((n (car lst)))
      (cond
        ((> n 7) '())
        ((even? n) (loop (cdr lst)))
        (else (begin
                (display "odd number:")
                (display " ")
                (display n)
                (newline)
                (loop (cdr lst))))))))
