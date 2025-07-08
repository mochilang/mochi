(define x 2)
(define label (let ((_t x)) (cond ((equal? _t 1) "one") ((equal? _t 2) "two") ((equal? _t 3) "three") (else "unknown"))))
(begin (display label) (newline))
