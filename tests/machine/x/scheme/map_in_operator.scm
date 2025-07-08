(define m (list (cons 1 "a") (cons 2 "b")))
(begin (display (if (assoc 1 m) #t #f)) (newline))
(begin (display (if (assoc 3 m) #t #f)) (newline))
