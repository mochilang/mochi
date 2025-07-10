(define math (list (cons 'pi 3.141592653589793) (cons 'sqrt (lambda (x) (sqrt x)))))
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

(begin (display ((map-get math 'sqrt) 16.0)) (newline))
(begin (display (map-get math 'pi)) (newline))
