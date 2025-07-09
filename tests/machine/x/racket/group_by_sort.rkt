#lang racket
(define items (list (hash 'cat "a" 'val 3) (hash 'cat "a" 'val 1) (hash 'cat "b" 'val 5) (hash 'cat "b" 'val 2)))
(define grouped (let ([groups (make-hash)])
  (for* ([i items]) (let ([key (hash-ref i 'cat)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'i i) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (> (let ([g a]) (- (apply + (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'val))))) (let ([g b]) (- (apply + (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'val)))))))))
  (for/list ([g _groups]) (hash 'cat (hash-ref g 'key) 'total (apply + (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'val)))))))
(displayln grouped)
