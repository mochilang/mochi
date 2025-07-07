(define (boom a b)
  (display "boom")
  (newline)
  #t)

(display (and #f (boom 1 2)))
(newline)
(display (or #t (boom 1 2)))
(newline)
