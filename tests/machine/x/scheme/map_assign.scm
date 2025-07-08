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

(define scores (list (cons "alice" 1)))
(set! scores (map-set scores "bob" 2))
(begin (display (map-get scores "bob")) (newline))
