#lang racket
(define products (list (hash 'name "Laptop" 'price 1500) (hash 'name "Smartphone" 'price 900) (hash 'name "Tablet" 'price 600) (hash 'name "Monitor" 'price 300) (hash 'name "Keyboard" 'price 100) (hash 'name "Mouse" 'price 50) (hash 'name "Headphones" 'price 200)))
(define expensive (for*/list ([p products]) p))
(displayln "--- Top products (excluding most expensive) ---")
(for ([item (if (hash? expensive) (hash-keys expensive) expensive)])
(displayln (string-join (map ~a (list (hash-ref item 'name) "costs $" (hash-ref item 'price))) " "))
)
