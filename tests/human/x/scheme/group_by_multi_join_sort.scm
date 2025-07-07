(define nation
  (list (list (cons 'n_nationkey 1) (cons 'n_name "BRAZIL"))))

(define customer
  (list (list (cons 'c_custkey 1)
              (cons 'c_name "Alice")
              (cons 'c_acctbal 100.0)
              (cons 'c_nationkey 1)
              (cons 'c_address "123 St")
              (cons 'c_phone "123-456")
              (cons 'c_comment "Loyal"))))

(define orders
  (list (list (cons 'o_orderkey 1000) (cons 'o_custkey 1) (cons 'o_orderdate "1993-10-15"))
        (list (cons 'o_orderkey 2000) (cons 'o_custkey 1) (cons 'o_orderdate "1994-01-02"))))

(define lineitem
  (list (list (cons 'l_orderkey 1000) (cons 'l_returnflag "R") (cons 'l_extendedprice 1000.0) (cons 'l_discount 0.1))
        (list (cons 'l_orderkey 2000) (cons 'l_returnflag "N") (cons 'l_extendedprice 500.0) (cons 'l_discount 0.0))))

(define start-date "1993-10-01")
(define end-date "1994-01-01")

(define rows '())
(for-each (lambda (c)
            (for-each (lambda (o)
                        (when (= (cdr (assoc 'o_custkey o)) (cdr (assoc 'c_custkey c)))
                          (for-each (lambda (l)
                                      (when (and (= (cdr (assoc 'l_orderkey l)) (cdr (assoc 'o_orderkey o)))
                                                 (string>=? (cdr (assoc 'o_orderdate o)) start-date)
                                                 (string<?  (cdr (assoc 'o_orderdate o)) end-date)
                                                 (string=? (cdr (assoc 'l_returnflag l)) "R"))
                                        (for-each (lambda (n)
                                                    (when (= (cdr (assoc 'n_nationkey n)) (cdr (assoc 'c_nationkey c)))
                                                      (set! rows (cons (list (cons 'c c) (cons 'o o) (cons 'l l) (cons 'n n)) rows))))
                                                  nation)))
                                  lineitem)))
                      orders))
          customer)

(define groups '())
(for-each (lambda (r)
            (let* ((c (cdr (assoc 'c r)))
                   (n (cdr (assoc 'n r)))
                   (key (list (cons 'c_custkey (cdr (assoc 'c_custkey c)))
                              (cons 'c_name (cdr (assoc 'c_name c)))
                              (cons 'c_acctbal (cdr (assoc 'c_acctbal c)))
                              (cons 'c_address (cdr (assoc 'c_address c)))
                              (cons 'c_phone (cdr (assoc 'c_phone c)))
                              (cons 'c_comment (cdr (assoc 'c_comment c)))
                              (cons 'n_name (cdr (assoc 'n_name n)))))
                   (entry (assoc key groups)))
              (if entry
                  (set-cdr! entry (cons r (cdr entry)))
                  (set! groups (cons (cons key (list r)) groups)))))
          rows)

(define result '())
(for-each (lambda (pair)
            (let* ((key (car pair))
                   (grp (cdr pair))
                   (revenue (apply +
                                   (map (lambda (x)
                                          (* (cdr (assoc 'l_extendedprice (cdr (assoc 'l x))))
                                             (- 1 (cdr (assoc 'l_discount (cdr (assoc 'l x)))))))
                                        grp))))
              (set! result
                    (cons (list
                           (cons 'c_custkey (cdr (assoc 'c_custkey key)))
                           (cons 'c_name (cdr (assoc 'c_name key)))
                           (cons 'revenue revenue)
                           (cons 'c_acctbal (cdr (assoc 'c_acctbal key)))
                           (cons 'n_name (cdr (assoc 'n_name key)))
                           (cons 'c_address (cdr (assoc 'c_address key)))
                           (cons 'c_phone (cdr (assoc 'c_phone key)))
                           (cons 'c_comment (cdr (assoc 'c_comment key))))
                          result))))
          groups)

(define sorted
  (sort result (lambda (a b)
                 (> (cdr (assoc 'revenue a)) (cdr (assoc 'revenue b))))))

(write sorted)
(newline)
