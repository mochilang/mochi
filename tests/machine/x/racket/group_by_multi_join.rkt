#lang racket
(define nations (list (hash 'id 1 'name "A") (hash 'id 2 'name "B")))
(define suppliers (list (hash 'id 1 'nation 1) (hash 'id 2 'nation 2)))
(define partsupp (list (hash 'part 100 'supplier 1 'cost 10 'qty 2) (hash 'part 100 'supplier 2 'cost 20 'qty 1) (hash 'part 200 'supplier 1 'cost 5 'qty 3)))
(define filtered (for*/list ([ps partsupp] #:when (string=? (hash-ref n 'name) "A")) (hash 'part (hash-ref ps 'part) 'value (* (hash-ref ps 'cost) (hash-ref ps 'qty)))))
(define grouped (for*/list ([x filtered]) (hash 'part (hash-ref g 'key) 'total (apply + (for*/list ([r g]) (hash-ref r 'value))))))
(displayln grouped)
