(define (make-person name age)
  (vector name age))
(define (person-name p) (vector-ref p 0))
(define (person-age p) (vector-ref p 1))

(define (make-book title author)
  (vector title author))
(define (book-title b) (vector-ref b 0))
(define (book-author b) (vector-ref b 1))

(define book (make-book "Go" (make-person "Bob" 42)))
(display (person-name (book-author book)))
(newline)
