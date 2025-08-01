;; Generated on 2025-07-21 21:13 +0700
(begin
  (current-error-port
    (open-output-string))
  (import
    (scheme base)
    (srfi 69)
    (scheme sort)
    (chibi string)))
(define
  (to-str x)
  (cond
    ((and
        (list? x)
        (pair? x)
        (pair?
          (car x))
        (string?
          (car
            (car x))))
      (string-append "{"
        (string-join
          (map
            (lambda
              (kv)
              (string-append "'"
                (car kv) "': "
                (to-str
                  (cdr kv)))) x) ", ") "}"))
    ((hash-table? x)
      (let
        ((pairs
            (hash-table->alist x)))
        (string-append "{"
          (string-join
            (map
              (lambda
                (kv)
                (string-append "'"
                  (car kv) "': "
                  (to-str
                    (cdr kv)))) pairs) ", ") "}")))
    ((list? x)
      (string-append "["
        (string-join
          (map to-str x) ", ") "]"))
    ((string? x)
      (string-append "'" x "'"))
    (else
      (number->string x))))
(define nation
  (list
    (alist->hash-table
      (list
        (cons "n_nationkey" 1)
        (cons "n_name" "BRAZIL")))))
(define customer
  (list
    (alist->hash-table
      (list
        (cons "c_custkey" 1)
        (cons "c_name" "Alice")
        (cons "c_acctbal" 100.0)
        (cons "c_nationkey" 1)
        (cons "c_address" "123 St")
        (cons "c_phone" "123-456")
        (cons "c_comment" "Loyal")))))
(define orders
  (list
    (alist->hash-table
      (list
        (cons "o_orderkey" 1000)
        (cons "o_custkey" 1)
        (cons "o_orderdate" "1993-10-15")))
    (alist->hash-table
      (list
        (cons "o_orderkey" 2000)
        (cons "o_custkey" 1)
        (cons "o_orderdate" "1994-01-02")))))
(define lineitem
  (list
    (alist->hash-table
      (list
        (cons "l_orderkey" 1000)
        (cons "l_returnflag" "R")
        (cons "l_extendedprice" 1000.0)
        (cons "l_discount" 0.1)))
    (alist->hash-table
      (list
        (cons "l_orderkey" 2000)
        (cons "l_returnflag" "N")
        (cons "l_extendedprice" 500.0)
        (cons "l_discount" 0.0)))))
(define start_date "1993-10-01")
(define end_date "1994-01-01")
(define result
  (let
    ((groups3
        (make-hash-table))
      (res6
        (list)))
    (begin
      (for-each
        (lambda
          (c)
          (for-each
            (lambda
              (o)
              (for-each
                (lambda
                  (l)
                  (for-each
                    (lambda
                      (n)
                      (if
                        (and
                          (and
                            (and
                              (and
                                (and
                                  (string>=?
                                    (hash-table-ref o "o_orderdate") start_date)
                                  (string<?
                                    (hash-table-ref o "o_orderdate") end_date))
                                (string=?
                                  (hash-table-ref l "l_returnflag") "R"))
                              (=
                                (hash-table-ref o "o_custkey")
                                (hash-table-ref c "c_custkey")))
                            (=
                              (hash-table-ref l "l_orderkey")
                              (hash-table-ref o "o_orderkey")))
                          (=
                            (hash-table-ref n "n_nationkey")
                            (hash-table-ref c "c_nationkey")))
                        (let*
                          ((k5
                              (alist->hash-table
                                (list
                                  (cons "c_custkey"
                                    (hash-table-ref c "c_custkey"))
                                  (cons "c_name"
                                    (hash-table-ref c "c_name"))
                                  (cons "c_acctbal"
                                    (hash-table-ref c "c_acctbal"))
                                  (cons "c_address"
                                    (hash-table-ref c "c_address"))
                                  (cons "c_phone"
                                    (hash-table-ref c "c_phone"))
                                  (cons "c_comment"
                                    (hash-table-ref c "c_comment"))
                                  (cons "n_name"
                                    (hash-table-ref n "n_name")))))
                            (g4
                              (hash-table-ref/default groups3 k5 #f)))
                          (begin
                            (if
                              (not g4)
                              (begin
                                (set! g4
                                  (alist->hash-table
                                    (list
                                      (cons "key" k5)
                                      (cons "items"
                                        (list)))))
                                (hash-table-set! groups3 k5 g4))
                              (quote nil))
                            (hash-table-set! g4 "items"
                              (append
                                (hash-table-ref g4 "items")
                                (list
                                  (alist->hash-table
                                    (list
                                      (cons "c" c)
                                      (cons "o" o)
                                      (cons "l" l)
                                      (cons "n" n))))))))
                        (quote nil))) nation)) lineitem)) orders)) customer)
      (for-each
        (lambda
          (g)
          (set! res6
            (append res6
              (list
                (list
                  (cons "c_custkey"
                    (hash-table-ref
                      (hash-table-ref g "key") "c_custkey"))
                  (cons "c_name"
                    (hash-table-ref
                      (hash-table-ref g "key") "c_name"))
                  (cons "revenue"
                    (apply +
                      (let
                        ((res1
                            (list)))
                        (begin
                          (for-each
                            (lambda
                              (x)
                              (set! res1
                                (append res1
                                  (list
                                    (*
                                      (hash-table-ref
                                        (hash-table-ref x "l") "l_extendedprice")
                                      (- 1
                                        (hash-table-ref
                                          (hash-table-ref x "l") "l_discount")))))))
                            (hash-table-ref g "items")) res1))))
                  (cons "c_acctbal"
                    (hash-table-ref
                      (hash-table-ref g "key") "c_acctbal"))
                  (cons "n_name"
                    (hash-table-ref
                      (hash-table-ref g "key") "n_name"))
                  (cons "c_address"
                    (hash-table-ref
                      (hash-table-ref g "key") "c_address"))
                  (cons "c_phone"
                    (hash-table-ref
                      (hash-table-ref g "key") "c_phone"))
                  (cons "c_comment"
                    (hash-table-ref
                      (hash-table-ref g "key") "c_comment")))))))
        (hash-table-values groups3))
      (set! res6
        (list-sort
          (lambda
            (a7 b8)
            (<
              (-
                (apply +
                  (let
                    ((res2
                        (list)))
                    (begin
                      (for-each
                        (lambda
                          (x)
                          (set! res2
                            (append res2
                              (list
                                (*
                                  (hash-table-ref
                                    (hash-table-ref x "l") "l_extendedprice")
                                  (- 1
                                    (hash-table-ref
                                      (hash-table-ref x "l") "l_discount")))))))
                        (hash-table-ref a7 "items")) res2))))
              (-
                (apply +
                  (let
                    ((res2
                        (list)))
                    (begin
                      (for-each
                        (lambda
                          (x)
                          (set! res2
                            (append res2
                              (list
                                (*
                                  (hash-table-ref
                                    (hash-table-ref x "l") "l_extendedprice")
                                  (- 1
                                    (hash-table-ref
                                      (hash-table-ref x "l") "l_discount")))))))
                        (hash-table-ref b8 "items")) res2)))))) res6)) res6)))
(display
  (to-str result))
(newline)
