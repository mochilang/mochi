#lang racket
(require racket/list)
(define items (list (hash 'n 1 'v "a") (hash 'n 1 'v "b") (hash 'n 2 'v "c")))
(define result (let ([_items0 (for*/list ([i items]) i)])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ([i a]) (hash-ref i 'n))) (string<? (let ([i a]) (hash-ref i 'n)) (let ([i b]) (hash-ref i 'n)))] [(string? (let ([i b]) (hash-ref i 'n))) (string<? (let ([i a]) (hash-ref i 'n)) (let ([i b]) (hash-ref i 'n)))] [else (< (let ([i a]) (hash-ref i 'n)) (let ([i b]) (hash-ref i 'n)))]))))
  (for/list ([i _items0]) (hash-ref i 'v))))
(displayln result)
