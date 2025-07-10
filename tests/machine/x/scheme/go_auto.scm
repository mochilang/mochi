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

(begin (display ((map-get testpkg 'Add) 2 3)) (newline))
(begin (display (map-get testpkg 'Pi)) (newline))
(begin (display (map-get testpkg 'Answer)) (newline))
