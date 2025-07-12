#lang racket
(define testpkg (hash 'Add (lambda (a b) (+ a b)) 'Pi 3.14 'Answer 42))
(displayln ((hash-ref testpkg 'Add) 2 3))
(displayln (hash-ref testpkg 'Pi))
(displayln (hash-ref testpkg 'Answer))
