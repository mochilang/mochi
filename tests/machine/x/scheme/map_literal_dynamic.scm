(define (map-get m k)
    (let ((p (assoc k m)))
        (if p (cdr p) '()))
)
(define (map-set m k v)
    (let ((p (assoc k m)))
        (if p
            (begin (set-cdr! p v) m)
            (cons (cons k v) m)))
)

(define x 3)
(define y 4)
(define m (list (cons "a" x) (cons "b" y)))
(begin (display (map-get m "a")) (display " ") (display (map-get m "b")) (newline))
