#lang racket
(define data
  (list (hash 'tag "a" 'val 1)
        (hash 'tag "a" 'val 2)
        (hash 'tag "b" 'val 3)))

;; group data by tag
(define groups (make-hash))
(for ([d data])
  (define tag (hash-ref d 'tag))
  (hash-set! groups tag (cons d (hash-ref groups tag '()))))

(define tmp '())
(for ([tag (hash-keys groups)])
  (define total (for/sum ([x (hash-ref groups tag)]) (hash-ref x 'val)))
  (set! tmp (append tmp (list (hash 'tag tag 'total total)))) )

(define result (sort tmp (lambda (a b) (string<? (hash-ref a 'tag) (hash-ref b 'tag)))))
(displayln result)
