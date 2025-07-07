#lang racket
(define people
  (list (hash 'name "Alice" 'age 30 'city "Paris")
        (hash 'name "Bob" 'age 15 'city "Hanoi")
        (hash 'name "Charlie" 'age 65 'city "Paris")
        (hash 'name "Diana" 'age 45 'city "Hanoi")
        (hash 'name "Eve" 'age 70 'city "Paris")
        (hash 'name "Frank" 'age 22 'city "Hanoi")))

;; build groups keyed by city
(define groups (make-hash))
(for ([p people])
  (define city (hash-ref p 'city))
  (define bucket (hash-ref groups city '()))
  (hash-set! groups city (cons p bucket)))

(define stats
  (for/list ([city (in-list (hash-keys groups))])
    (define entries (hash-ref groups city))
    (define count (length entries))
    (define total (for/sum ([e entries]) (hash-ref e 'age)))
    (hash 'city city 'count count 'avg_age (/ total count))))

(displayln "--- People grouped by city ---")
(for ([s stats])
  (displayln
   (format "~a: count = ~a, avg_age = ~a"
           (hash-ref s 'city)
           (hash-ref s 'count)
           (hash-ref s 'avg_age))))
