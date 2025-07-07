(define (make-counter n)
  (vector n))
(define (counter-n c)
  (vector-ref c 0))
(define (set-counter-n! c val)
  (vector-set! c 0 val))

(define (inc c)
  (set-counter-n! c (+ (counter-n c) 1)))

(define c (make-counter 0))
(inc c)
(display (counter-n c))
(newline)
