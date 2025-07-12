#lang racket
(require racket/list)
(define data (list (hash 'a 1 'b 2) (hash 'a 1 'b 1) (hash 'a 0 'b 5)))
(define sorted (let ([_items0 (for*/list ([x data]) x)])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ([x a]) (hash 'a (hash-ref x 'a) 'b (hash-ref x 'b)))) (string<? (let ([x a]) (hash 'a (hash-ref x 'a) 'b (hash-ref x 'b))) (let ([x b]) (hash 'a (hash-ref x 'a) 'b (hash-ref x 'b))))] [(string? (let ([x b]) (hash 'a (hash-ref x 'a) 'b (hash-ref x 'b)))) (string<? (let ([x a]) (hash 'a (hash-ref x 'a) 'b (hash-ref x 'b))) (let ([x b]) (hash 'a (hash-ref x 'a) 'b (hash-ref x 'b))))] [else (< (let ([x a]) (hash 'a (hash-ref x 'a) 'b (hash-ref x 'b))) (let ([x b]) (hash 'a (hash-ref x 'a) 'b (hash-ref x 'b))))]))))
  (for/list ([x _items0]) x)))
(displayln sorted)
