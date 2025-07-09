#lang racket
(require json)
(define people (list (hash 'name "Alice" 'city "Paris") (hash 'name "Bob" 'city "Hanoi") (hash 'name "Charlie" 'city "Paris") (hash 'name "Diana" 'city "Hanoi") (hash 'name "Eve" 'city "Paris") (hash 'name "Frank" 'city "Hanoi") (hash 'name "George" 'city "Paris")))
(define big (let ([groups (make-hash)])
  (for* ([p people]) (let ([key (hash-ref p 'city)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'p p) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (filter (lambda (g) (>= (length (hash-ref g 'items)) 4)) _groups))
  (for/list ([g _groups]) (hash 'city (hash-ref g 'key) 'num (length (hash-ref g 'items))))))
(displayln (jsexpr->string big))
