(define items '(((n . 1) (v . "a"))
               ((n . 1) (v . "b"))
               ((n . 2) (v . "c"))))

(define (get-n item) (cdr (assoc 'n item)))
(define (get-v item) (cdr (assoc 'v item)))

(define sorted (sort items (lambda (a b) (< (get-n a) (get-n b)))))
(define result (map get-v sorted))
(display result)
(newline)
