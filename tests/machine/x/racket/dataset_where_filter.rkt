#lang racket
(define people (list (hash 'name "Alice" 'age 30) (hash 'name "Bob" 'age 15) (hash 'name "Charlie" 'age 65) (hash 'name "Diana" 'age 45)))
(define adults (for*/list ([person people] #:when (>= (hash-ref person 'age) 18)) (hash 'name (hash-ref person 'name) 'age (hash-ref person 'age) 'is_senior (>= (hash-ref person 'age) 60))))
(displayln "--- Adults ---")
(for ([person (if (hash? adults) (hash-keys adults) adults)])
(displayln (string-join (map ~a (list (hash-ref person 'name) "is" (hash-ref person 'age) (if (hash-ref person 'is_senior) " (senior)" ""))) " "))
)
