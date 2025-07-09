#lang racket
(define people (list (hash 'name "Alice" 'city "Paris") (hash 'name "Bob" 'city "Hanoi") (hash 'name "Charlie" 'city "Paris") (hash 'name "Diana" 'city "Hanoi") (hash 'name "Eve" 'city "Paris") (hash 'name "Frank" 'city "Hanoi") (hash 'name "George" 'city "Paris")))
(define big (for*/list ([p people]) (hash 'city (hash-ref g 'key) 'num (length g))))
(json big)
