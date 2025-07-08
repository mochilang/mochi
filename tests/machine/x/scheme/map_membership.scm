(define m (list (cons "a" 1) (cons "b" 2)))
(begin (display (if (assoc "a" m) #t #f)) (newline))
(begin (display (if (assoc "c" m) #t #f)) (newline))
