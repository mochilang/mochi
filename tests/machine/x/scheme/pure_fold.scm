; Generated by Mochi compiler v0.10.27 on 2025-07-17T18:04:18Z
(define (triple x)
  (call/cc (lambda (return)
    (return (* x 3))
  ))
)

(begin (display (triple (+ 1 2))) (newline))
