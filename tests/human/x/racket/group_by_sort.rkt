#lang racket
(define items
  (list (hash 'cat "a" 'val 3)
        (hash 'cat "a" 'val 1)
        (hash 'cat "b" 'val 5)
        (hash 'cat "b" 'val 2)))

(define groups (make-hash))
(for ([i items])
  (define cat (hash-ref i 'cat))
  (hash-set! groups cat (cons i (hash-ref groups cat '()))))

(define result
  (for/list ([cat (hash-keys groups)])
    (define vals (hash-ref groups cat))
    (define total (for/sum ([v vals]) (hash-ref v 'val)))
    (hash 'cat cat 'total total)))

(define sorted
  (sort result (lambda (a b) (> (hash-ref a 'total) (hash-ref b 'total)))))
(displayln sorted)
