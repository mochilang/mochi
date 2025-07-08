(define xs (list 1 2 3))
(begin (display (if (member 2 xs) #t #f)) (newline))
(begin (display (not (if (member 5 xs) #t #f))) (newline))
