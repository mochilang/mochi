(define (outer x)
  (define (inner y)
    (+ x y))
  (inner 5))

(display (outer 3))
(newline)
