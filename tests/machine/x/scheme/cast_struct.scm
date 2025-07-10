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

(define (new-Todo title)
  (list (cons 'title title))
)

(define todo (let ((_tmp (list (cons "title" "hi")))) (new-Todo (map-get _tmp "title"))))
(begin (display (map-get todo 'title)) (newline))
