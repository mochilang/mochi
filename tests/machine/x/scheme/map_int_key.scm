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

(define m (list (cons 1 "a") (cons 2 "b")))
(begin (display (map-get m 1)) (newline))
