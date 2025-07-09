#lang racket
(define people (list (hash 'name "Alice" 'age 30 'city "Paris") (hash 'name "Bob" 'age 15 'city "Hanoi") (hash 'name "Charlie" 'age 65 'city "Paris") (hash 'name "Diana" 'age 45 'city "Hanoi") (hash 'name "Eve" 'age 70 'city "Paris") (hash 'name "Frank" 'age 22 'city "Hanoi")))
(define stats (for*/list ([person people]) (hash 'city (hash-ref g 'key) 'count (length g) 'avg_age (let ([xs (for*/list ([p g]) (hash-ref p 'age))]) (/ (apply + xs) (length xs))))))
(displayln "--- People grouped by city ---")
(for ([s (if (hash? stats) (hash-keys stats) stats)])
(displayln (string-join (map ~a (list (hash-ref s 'city) ": count =" (hash-ref s 'count) ", avg_age =" (hash-ref s 'avg_age))) " "))
)
