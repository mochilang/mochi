#lang racket
(define items (list (hash 'cat "a" 'val 10 'flag #t) (hash 'cat "a" 'val 5 'flag #f) (hash 'cat "b" 'val 20 'flag #t)))
(define result (let ([groups (make-hash)])
  (for* ([i items]) (let* ([key (hash-ref i 'cat)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons i bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (> (let ([g a]) (hash-ref g 'key)) (let ([g b]) (hash-ref g 'key))))))
  (for/list ([g _groups]) (hash 'cat (hash-ref g 'key) 'share (/ (apply + (for*/list ([x (hash-ref g 'items)]) (if (hash-ref x 'flag) (hash-ref x 'val) 0))) (apply + (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'val))))))))
(displayln result)
