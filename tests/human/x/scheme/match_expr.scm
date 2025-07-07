(define x 2)
(define label
  (cond ((= x 1) "one")
        ((= x 2) "two")
        ((= x 3) "three")
        (else "unknown")))
(display label)
(newline)
