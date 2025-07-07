#lang racket
(struct Person (name age status) #:transparent)

(define people
  (list (Person "Alice" 17 "minor")
        (Person "Bob" 25 "unknown")
        (Person "Charlie" 18 "unknown")
        (Person "Diana" 16 "minor")))

(for ([p people])
  (when (>= (Person-age p) 18)
    (set-Person-status! p "adult")
    (set-Person-age! p (add1 (Person-age p)))))

(define expected
  (list (Person "Alice" 17 "minor")
        (Person "Bob" 26 "adult")
        (Person "Charlie" 19 "adult")
        (Person "Diana" 16 "minor")))

(define (persons-equal? xs ys)
  (and (= (length xs) (length ys))
       (for/and ([a xs] [b ys]) (equal? a b))))

(when (persons-equal? people expected)
  (displayln "ok"))
