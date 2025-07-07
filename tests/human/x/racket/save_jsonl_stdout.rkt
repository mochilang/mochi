#lang racket
(require json)
(define people
  (list (hash 'name "Alice" 'age 30)
        (hash 'name "Bob" 'age 25)))
(for ([p people])
  (displayln (jsexpr->string p)))
