#lang racket
(define people
  (list (hash 'name "Alice" 'city "Paris")
        (hash 'name "Bob" 'city "Hanoi")
        (hash 'name "Charlie" 'city "Paris")
        (hash 'name "Diana" 'city "Hanoi")
        (hash 'name "Eve" 'city "Paris")
        (hash 'name "Frank" 'city "Hanoi")
        (hash 'name "George" 'city "Paris")))

;; group by city
(define groups (make-hash))
(for ([p people])
  (define city (hash-ref p 'city))
  (hash-set! groups city (cons p (hash-ref groups city '()))))

(define big
  (for/list ([city (hash-keys groups)] #:when (>= (length (hash-ref groups city)) 4))
    (hash 'city city 'num (length (hash-ref groups city)))) )

(require json)
(displayln (jsexpr->string big))
