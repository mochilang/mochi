#lang racket
(require racket/list)
(require json)
(define region (list (hash 'r_regionkey 0 'r_name "AMERICA")))
(define nation (list (hash 'n_nationkey 10 'n_regionkey 0 'n_name "BRAZIL") (hash 'n_nationkey 20 'n_regionkey 0 'n_name "CANADA")))
(define customer (list (hash 'c_custkey 1 'c_nationkey 10) (hash 'c_custkey 2 'c_nationkey 20)))
(define orders (list (hash 'o_orderkey 100 'o_custkey 1 'o_orderdate "1995-04-10") (hash 'o_orderkey 200 'o_custkey 2 'o_orderdate "1995-07-15")))
(define lineitem (list (hash 'l_orderkey 100 'l_suppkey 1000 'l_partkey 5000 'l_extendedprice 1000 'l_discount 0.1) (hash 'l_orderkey 200 'l_suppkey 2000 'l_partkey 5000 'l_extendedprice 500 'l_discount 0.05)))
(define supplier (list (hash 's_suppkey 1000) (hash 's_suppkey 2000)))
(define part (list (hash 'p_partkey 5000 'p_type "ECONOMY ANODIZED STEEL") (hash 'p_partkey 6000 'p_type "SMALL BRASS")))
(define start_date "1995-01-01")
(define end_date "1996-12-31")
(define target_type "ECONOMY ANODIZED STEEL")
(define target_nation "BRAZIL")
(define result (let ([groups (make-hash)])
  (for* ([l lineitem] [p part] [s supplier] [o orders] [c customer] [n nation] [r region] #:when (and (equal? (hash-ref p 'p_partkey) (hash-ref l 'l_partkey)) (equal? (hash-ref s 's_suppkey) (hash-ref l 'l_suppkey)) (equal? (hash-ref o 'o_orderkey) (hash-ref l 'l_orderkey)) (equal? (hash-ref c 'c_custkey) (hash-ref o 'o_custkey)) (equal? (hash-ref n 'n_nationkey) (hash-ref c 'c_nationkey)) (equal? (hash-ref r 'r_regionkey) (hash-ref n 'n_regionkey)) (and (and (and (string=? (hash-ref p 'p_type) target_type) (string>=? (hash-ref o 'o_orderdate) start_date)) (string<=? (hash-ref o 'o_orderdate) end_date)) (string=? (hash-ref r 'r_name) "AMERICA")))) (let* ([key (substring (hash-ref o 'o_orderdate) 0 4)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'l l 'p p 's s 'o o 'c c 'n n 'r r) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([year a]) (hash-ref year 'key))) (string>? (let ([year a]) (hash-ref year 'key)) (let ([year b]) (hash-ref year 'key)))] [(string? (let ([year b]) (hash-ref year 'key))) (string>? (let ([year a]) (hash-ref year 'key)) (let ([year b]) (hash-ref year 'key)))] [else (> (let ([year a]) (hash-ref year 'key)) (let ([year b]) (hash-ref year 'key)))]))))
  (for/list ([year _groups]) (hash 'o_year (hash-ref year 'key) 'mkt_share (/ (apply + (for*/list ([v (for*/list ([x (hash-ref year 'items)]) (match (string=? (hash-ref (hash-ref x 'n) 'n_name) target_nation) [#t (* (hash-ref (hash-ref x 'l) 'l_extendedprice) (- 1 (hash-ref (hash-ref x 'l) 'l_discount)))] [_ 0]))]) v)) (apply + (for*/list ([v (for*/list ([x (hash-ref year 'items)]) (* (hash-ref (hash-ref x 'l) 'l_extendedprice) (- 1 (hash-ref (hash-ref x 'l) 'l_discount))))]) v)))))))
(displayln (jsexpr->string result))
(define numerator (* 1000 0.9))
(define denominator (+ numerator (* 500 0.95)))
(define share (/ numerator denominator))
(when (equal? result (list (hash 'o_year "1995" 'mkt_share share))) (displayln "ok"))
