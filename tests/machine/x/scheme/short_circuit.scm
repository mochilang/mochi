(define (boom a b)
  (call/cc (lambda (return)
    (begin (display "boom") (newline))
    (return #t)
  ))
)

(begin (display (and #f (boom 1 2))) (newline))
(begin (display (or #t (boom 1 2))) (newline))
