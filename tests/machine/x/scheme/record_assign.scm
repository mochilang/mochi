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

(define (new-Counter n)
  (list (cons 'n n))
)

(define (inc c)
  (call/cc (lambda (return)
    (set! c (+ (map-get c "n") 1))
  ))
)

(define c (list (cons 'n 0)))
(inc c)
(begin (display (map-get c "n")) (newline))
