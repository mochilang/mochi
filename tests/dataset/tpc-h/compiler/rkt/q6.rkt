#lang racket
(require json)
(define lineitem (list (hash 'l_extendedprice 1000 'l_discount 0.06 'l_shipdate "1994-02-15" 'l_quantity 10) (hash 'l_extendedprice 500 'l_discount 0.07 'l_shipdate "1994-03-10" 'l_quantity 23) (hash 'l_extendedprice 400 'l_discount 0.04 'l_shipdate "1994-04-10" 'l_quantity 15) (hash 'l_extendedprice 200 'l_discount 0.06 'l_shipdate "1995-01-01" 'l_quantity 5)))
(define result (for/sum ([l lineitem] #:when (and (and (and (and (and (string>=? (hash-ref l 'l_shipdate) "1994-01-01") (string<? (hash-ref l 'l_shipdate) "1995-01-01")) (>= (hash-ref l 'l_discount) 0.05)) (<= (hash-ref l 'l_discount) 0.07)) (< (hash-ref l 'l_quantity) 24)))) (* (hash-ref l 'l_extendedprice) (hash-ref l 'l_discount))))
(displayln (jsexpr->string result))
(when (equal? result (+ (* 1000 0.06) (* 500 0.07))) (displayln "ok"))
