; Generated by Mochi compiler v0.10.27 on 2025-07-17T18:04:06Z
(define label '())
(define x '())
(set! x 2)
(set! label (let ((_t x)) (cond ((equal? _t 1) "one") ((equal? _t 2) "two") ((equal? _t 3) "three") (else "unknown"))))
(begin (display label) (newline))
