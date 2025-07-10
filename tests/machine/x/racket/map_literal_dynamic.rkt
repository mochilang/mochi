#lang racket
(require racket/list)
(define x 3)
(define y 4)
(define m (hash "a" x "b" y))
(displayln (string-join (map ~a (list (cond [(string? m) (string-ref m "a")] [(hash? m) (hash-ref m "a")] [else (list-ref m "a")]) (cond [(string? m) (string-ref m "b")] [(hash? m) (hash-ref m "b")] [else (list-ref m "b")]))) " "))
