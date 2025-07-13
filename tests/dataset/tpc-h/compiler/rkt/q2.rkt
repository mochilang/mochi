#lang racket
(require racket/list)
(require json)
(define (_date_number s)
  (let ([parts (string-split s "-")])
    (if (= (length parts) 3)
        (+ (* (string->number (list-ref parts 0)) 10000)
           (* (string->number (list-ref parts 1)) 100)
           (string->number (list-ref parts 2)))
        #f)))

(define (_to_string v) (format "~a" v))

(define (_lt a b)
  (cond
    [(and (number? a) (number? b)) (< a b)]
    [(and (string? a) (string? b))
     (let ([da (_date_number a)]
           [db (_date_number b)])
       (if (and da db)
           (< da db)
           (string<? a b)))]
    [(and (list? a) (list? b))
     (cond [(null? a) (not (null? b))]
           [(null? b) #f]
           [else (let ([ka (car a)] [kb (car b)])
                   (if (equal? ka kb)
                       (_lt (cdr a) (cdr b))
                       (_lt ka kb)))])]
    [else (string<? (_to_string a) (_to_string b))]))

(define (_gt a b) (_lt b a))
(define (_le a b) (or (_lt a b) (equal? a b)))
(define (_ge a b) (or (_gt a b) (equal? a b)))

(define (_min v)
  (let* ([lst (cond [(and (hash? v) (hash-has-key? v 'items)) (hash-ref v 'items)]
                    [(list? v) v]
                    [else '()])]
         [m 0])
    (when (not (null? lst))
      (set! m (car lst))
      (for ([n (cdr lst)])
        (when (_lt n m) (set! m n))))
    m))

(define (_max v)
  (let* ([lst (cond [(and (hash? v) (hash-has-key? v 'items)) (hash-ref v 'items)]
                    [(list? v) v]
                    [else '()])]
         [m 0])
    (when (not (null? lst))
      (set! m (car lst))
      (for ([n (cdr lst)])
        (when (_gt n m) (set! m n))))
    m))

(define region (list (hash 'r_regionkey 1 'r_name "EUROPE") (hash 'r_regionkey 2 'r_name "ASIA")))
(define nation (list (hash 'n_nationkey 10 'n_regionkey 1 'n_name "FRANCE") (hash 'n_nationkey 20 'n_regionkey 2 'n_name "CHINA")))
(define supplier (list (hash 's_suppkey 100 's_name "BestSupplier" 's_address "123 Rue" 's_nationkey 10 's_phone "123" 's_acctbal 1000 's_comment "Fast and reliable") (hash 's_suppkey 200 's_name "AltSupplier" 's_address "456 Way" 's_nationkey 20 's_phone "456" 's_acctbal 500 's_comment "Slow")))
(define part (list (hash 'p_partkey 1000 'p_type "LARGE BRASS" 'p_size 15 'p_mfgr "M1") (hash 'p_partkey 2000 'p_type "SMALL COPPER" 'p_size 15 'p_mfgr "M2")))
(define partsupp (list (hash 'ps_partkey 1000 'ps_suppkey 100 'ps_supplycost 10) (hash 'ps_partkey 1000 'ps_suppkey 200 'ps_supplycost 15)))
(define europe_nations (for*/list ([r region] [n nation] #:when (and (equal? (hash-ref n 'n_regionkey) (hash-ref r 'r_regionkey)) (string=? (hash-ref r 'r_name) "EUROPE"))) n))
(define europe_suppliers (for*/list ([s supplier] [n europe_nations] #:when (and (equal? (hash-ref s 's_nationkey) (hash-ref n 'n_nationkey)))) (hash 's s 'n n)))
(define target_parts (for*/list ([p part] #:when (and (and (equal? (hash-ref p 'p_size) 15) (string=? (hash-ref p 'p_type) "LARGE BRASS")))) p))
(define target_partsupp (for*/list ([ps partsupp] [p target_parts] [s europe_suppliers] #:when (and (equal? (hash-ref ps 'ps_partkey) (hash-ref p 'p_partkey)) (equal? (hash-ref ps 'ps_suppkey) (hash-ref (hash-ref s 's) 's_suppkey)))) (hash 's_acctbal (hash-ref (hash-ref s 's) 's_acctbal) 's_name (hash-ref (hash-ref s 's) 's_name) 'n_name (hash-ref (hash-ref s 'n) 'n_name) 'p_partkey (hash-ref p 'p_partkey) 'p_mfgr (hash-ref p 'p_mfgr) 's_address (hash-ref (hash-ref s 's) 's_address) 's_phone (hash-ref (hash-ref s 's) 's_phone) 's_comment (hash-ref (hash-ref s 's) 's_comment) 'ps_supplycost (hash-ref ps 'ps_supplycost))))
(define costs (for*/list ([x target_partsupp]) (hash-ref x 'ps_supplycost)))
(define min_cost (_min costs))
(define result (let ([_items0 (for*/list ([x target_partsupp] #:when (and (equal? (hash-ref x 'ps_supplycost) min_cost))) (hash 'x x))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((x (hash-ref a 'x))) (hash-ref x 's_acctbal))) (string>? (let ((x (hash-ref a 'x))) (hash-ref x 's_acctbal)) (let ((x (hash-ref b 'x))) (hash-ref x 's_acctbal)))] [(string? (let ((x (hash-ref b 'x))) (hash-ref x 's_acctbal))) (string>? (let ((x (hash-ref a 'x))) (hash-ref x 's_acctbal)) (let ((x (hash-ref b 'x))) (hash-ref x 's_acctbal)))] [else (> (let ((x (hash-ref a 'x))) (hash-ref x 's_acctbal)) (let ((x (hash-ref b 'x))) (hash-ref x 's_acctbal)))]))))
  (for/list ([item _items0]) (let ((x (hash-ref item 'x))) x))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 's_acctbal 1000 's_name "BestSupplier" 'n_name "FRANCE" 'p_partkey 1000 'p_mfgr "M1" 's_address "123 Rue" 's_phone "123" 's_comment "Fast and reliable" 'ps_supplycost 10))) (displayln "ok"))
