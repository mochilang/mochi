#lang racket
(define products
  (list (hash 'name "Laptop" 'price 1500)
        (hash 'name "Smartphone" 'price 900)
        (hash 'name "Tablet" 'price 600)
        (hash 'name "Monitor" 'price 300)
        (hash 'name "Keyboard" 'price 100)
        (hash 'name "Mouse" 'price 50)
        (hash 'name "Headphones" 'price 200)))
(define sorted
  (sort products (lambda (a b) (> (hash-ref a 'price) (hash-ref b 'price)))))
(define expensive (take (rest sorted) 3))
(displayln "--- Top products (excluding most expensive) ---")
(for ([item expensive])
  (displayln (string-append (hash-ref item 'name)
                            " costs $"
                            (number->string (hash-ref item 'price)))))
