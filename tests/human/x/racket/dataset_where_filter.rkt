#lang racket
(define people
  (list (hash 'name "Alice" 'age 30)
        (hash 'name "Bob" 'age 15)
        (hash 'name "Charlie" 'age 65)
        (hash 'name "Diana" 'age 45)))
(define adults
  (for/list ([p people] #:when (>= (hash-ref p 'age) 18))
    (hash 'name (hash-ref p 'name)
          'age (hash-ref p 'age)
          'is_senior (>= (hash-ref p 'age) 60))))
(displayln "--- Adults ---")
(for ([person adults])
  (define suffix (if (hash-ref person 'is_senior) " (senior)" ""))
  (displayln (string-append (hash-ref person 'name)
                            " is "
                            (number->string (hash-ref person 'age))
                            suffix)))
