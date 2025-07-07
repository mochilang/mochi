#lang racket
(define items
  (list (hash 'cat "a" 'val 10 'flag #t)
        (hash 'cat "a" 'val 5  'flag #f)
        (hash 'cat "b" 'val 20 'flag #t)))

(define groups (make-hash))
(for ([it items])
  (define cat (hash-ref it 'cat))
  (define g (hash-ref groups cat (hash 'num 0 'den 0)))
  (hash-set! g 'den (+ (hash-ref g 'den) (hash-ref it 'val)))
  (when (hash-ref it 'flag)
    (hash-set! g 'num (+ (hash-ref g 'num) (hash-ref it 'val))))
  (hash-set! groups cat g))

(define result
  (for/list ([cat (sort (hash-keys groups) string<?)])
    (define g (hash-ref groups cat))
    (define share (/ (hash-ref g 'num) (hash-ref g 'den)))
    (hash 'cat cat 'share share)))

(displayln result)
