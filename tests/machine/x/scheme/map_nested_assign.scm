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

(define data (list (cons "outer" (list (cons "inner" 1)))))
(set! data (map-set data "outer" (map-set (map-get data "outer") "inner" 2)))
(begin (display (map-get (map-get data "outer") "inner")) (newline))
