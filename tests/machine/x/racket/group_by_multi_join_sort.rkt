#lang racket
(define nation (list (hash 'n_nationkey 1 'n_name "BRAZIL")))
(define customer
  (list (hash 'c_custkey 1 'c_name "Alice" 'c_acctbal 100.0 'c_nationkey 1
              'c_address "123 St" 'c_phone "123-456" 'c_comment "Loyal")))
(define orders
  (list (hash 'o_orderkey 1000 'o_custkey 1 'o_orderdate "1993-10-15")
        (hash 'o_orderkey 2000 'o_custkey 1 'o_orderdate "1994-01-02")))
(define lineitem
  (list (hash 'l_orderkey 1000 'l_returnflag "R" 'l_extendedprice 1000.0 'l_discount 0.1)
        (hash 'l_orderkey 2000 'l_returnflag "N" 'l_extendedprice 500.0 'l_discount 0.0)))

(define start-date "1993-10-01")
(define end-date "1994-01-01")

;; build filtered rows
(define rows
  (for*/list ([c customer]
              [o orders #:when (= (hash-ref o 'o_custkey) (hash-ref c 'c_custkey))]
              [l lineitem #:when (= (hash-ref l 'l_orderkey) (hash-ref o 'o_orderkey))]
              [n nation #:when (= (hash-ref n 'n_nationkey) (hash-ref c 'c_nationkey))]
              #:when (and (string>=? (hash-ref o 'o_orderdate) start-date)
                          (string<? (hash-ref o 'o_orderdate) end-date)
                          (string=? (hash-ref l 'l_returnflag) "R"))]
              )
    (hash 'c c 'o o 'l l 'n n)))

;; group by customer attributes and nation name
(define groups (make-hash))
(for ([r rows])
  (define key (list (hash-ref (hash-ref r 'c) 'c_custkey)
                    (hash-ref (hash-ref r 'c) 'c_name)
                    (hash-ref (hash-ref r 'c) 'c_acctbal)
                    (hash-ref (hash-ref r 'c) 'c_address)
                    (hash-ref (hash-ref r 'c) 'c_phone)
                    (hash-ref (hash-ref r 'c) 'c_comment)
                    (hash-ref (hash-ref r 'n) 'n_name)))
  (hash-set! groups key (cons r (hash-ref groups key '()))))

(define result
  (for/list ([key (hash-keys groups)])
    (define grp (hash-ref groups key))
    (define revenue
      (for/sum ([x grp])
        (* (hash-ref (hash-ref x 'l) 'l_extendedprice)
           (- 1 (hash-ref (hash-ref x 'l) 'l_discount)))) )
    (hash 'c_custkey (list-ref key 0)
          'c_name (list-ref key 1)
          'revenue revenue
          'c_acctbal (list-ref key 2)
          'n_name (list-ref key 6)
          'c_address (list-ref key 3)
          'c_phone (list-ref key 4)
          'c_comment (list-ref key 5))))

(define sorted (sort result (lambda (a b) (> (hash-ref a 'revenue) (hash-ref b 'revenue)))))
(displayln sorted)
