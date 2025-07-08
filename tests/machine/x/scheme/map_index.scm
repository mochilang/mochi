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

(define m (list (cons "a" 1) (cons "b" 2)))
(begin (display (map-get m "b")) (newline))
