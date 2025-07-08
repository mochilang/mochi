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

(define (new-Person name age)
  (list (cons 'name name) (cons 'age age))
)

(define (new-Book title author)
  (list (cons 'title title) (cons 'author author))
)

(define book (list (cons 'title "Go") (cons 'author (list (cons 'name "Bob") (cons 'age 42)))))
(begin (display (map-get (map-get book "author") "name")) (newline))
