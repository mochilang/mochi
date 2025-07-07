#lang racket
(define nations (list (hash 'id 1 'name "A")
                      (hash 'id 2 'name "B")))
(define suppliers (list (hash 'id 1 'nation 1)
                        (hash 'id 2 'nation 2)))
(define partsupp
  (list (hash 'part 100 'supplier 1 'cost 10.0 'qty 2)
        (hash 'part 100 'supplier 2 'cost 20.0 'qty 1)
        (hash 'part 200 'supplier 1 'cost 5.0  'qty 3)))

(define filtered
  (for/list ([ps partsupp]
             #:when (let ([s (findf (lambda (x) (= (hash-ref x 'id) (hash-ref ps 'supplier))) suppliers)]
                          [n (findf (lambda (x) (= (hash-ref x 'id) (hash-ref s 'nation))) nations)])
                      (string=? (hash-ref n 'name) "A")))
    (hash 'part (hash-ref ps 'part)
          'value (* (hash-ref ps 'cost) (hash-ref ps 'qty)))))

(define groups (make-hash))
(for ([x filtered])
  (define part (hash-ref x 'part))
  (hash-set! groups part (cons x (hash-ref groups part '()))))

(define result
  (for/list ([part (sort (hash-keys groups) <)])
    (define items (hash-ref groups part))
    (define total (for/sum ([r items]) (hash-ref r 'value)))
    (hash 'part part 'total total)))

(displayln result)
