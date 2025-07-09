#lang racket
(define nation (list (hash 'n_nationkey 1 'n_name "BRAZIL")))
(define customer (list (hash 'c_custkey 1 'c_name "Alice" 'c_acctbal 100 'c_nationkey 1 'c_address "123 St" 'c_phone "123-456" 'c_comment "Loyal")))
(define orders (list (hash 'o_orderkey 1000 'o_custkey 1 'o_orderdate "1993-10-15") (hash 'o_orderkey 2000 'o_custkey 1 'o_orderdate "1994-01-02")))
(define lineitem (list (hash 'l_orderkey 1000 'l_returnflag "R" 'l_extendedprice 1000 'l_discount 0.1) (hash 'l_orderkey 2000 'l_returnflag "N" 'l_extendedprice 500 'l_discount 0)))
(define start_date "1993-10-01")
(define end_date "1994-01-01")
(define result (for*/list ([c customer] #:when (string=? (and (< (and (>= (hash-ref o 'o_orderdate) start_date) (hash-ref o 'o_orderdate)) end_date) (hash-ref l 'l_returnflag)) "R")) (hash 'c_custkey (hash-ref (hash-ref g 'key) 'c_custkey) 'c_name (hash-ref (hash-ref g 'key) 'c_name) 'revenue (apply + (for*/list ([x g]) (* (hash-ref (hash-ref x 'l) 'l_extendedprice) (- 1 (hash-ref (hash-ref x 'l) 'l_discount))))) 'c_acctbal (hash-ref (hash-ref g 'key) 'c_acctbal) 'n_name (hash-ref (hash-ref g 'key) 'n_name) 'c_address (hash-ref (hash-ref g 'key) 'c_address) 'c_phone (hash-ref (hash-ref g 'key) 'c_phone) 'c_comment (hash-ref (hash-ref g 'key) 'c_comment))))
(displayln result)
