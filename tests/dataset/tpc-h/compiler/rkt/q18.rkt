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

(define (_json-fix v)
  (cond
    [(and (number? v) (rational? v) (not (integer? v))) (real->double-flonum v)]
    [(list? v) (map _json-fix v)]
    [(hash? v) (for/hash ([(k val) v]) (values k (_json-fix val)))]
    [else v]))

(define nation (list (hash 'n_nationkey 1 'n_name "GERMANY")))
(define customer (list (hash 'c_custkey 1 'c_name "Alice" 'c_acctbal 1000 'c_nationkey 1 'c_address "123 Market St" 'c_phone "123-456" 'c_comment "Premium client") (hash 'c_custkey 2 'c_name "Bob" 'c_acctbal 200 'c_nationkey 1 'c_address "456 Side St" 'c_phone "987-654" 'c_comment "Frequent returns")))
(define orders (list (hash 'o_orderkey 100 'o_custkey 1) (hash 'o_orderkey 200 'o_custkey 1) (hash 'o_orderkey 300 'o_custkey 2)))
(define lineitem (list (hash 'l_orderkey 100 'l_quantity 150 'l_extendedprice 1000 'l_discount 0.1) (hash 'l_orderkey 200 'l_quantity 100 'l_extendedprice 800 'l_discount 0) (hash 'l_orderkey 300 'l_quantity 30 'l_extendedprice 300 'l_discount 0.05)))
(define threshold 200)
(define result (let ([groups (make-hash)])
  (for* ([c customer] [o orders] [l lineitem] [n nation] #:when (and (equal? (hash-ref o 'o_custkey) (hash-ref c 'c_custkey)) (equal? (hash-ref l 'l_orderkey) (hash-ref o 'o_orderkey)) (equal? (hash-ref n 'n_nationkey) (hash-ref c 'c_nationkey)))) (let* ([key (hash 'c_name (hash-ref c 'c_name) 'c_custkey (hash-ref c 'c_custkey) 'c_acctbal (hash-ref c 'c_acctbal) 'c_address (hash-ref c 'c_address) 'c_phone (hash-ref c 'c_phone) 'c_comment (hash-ref c 'c_comment) 'n_name (hash-ref n 'n_name))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'c c 'o o 'l l 'n n) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (* (hash-ref (hash-ref x 'l) 'l_extendedprice) (- 1 (hash-ref (hash-ref x 'l) 'l_discount))))]) v))))) (string>? (let ([g a]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (* (hash-ref (hash-ref x 'l) 'l_extendedprice) (- 1 (hash-ref (hash-ref x 'l) 'l_discount))))]) v)))) (let ([g b]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (* (hash-ref (hash-ref x 'l) 'l_extendedprice) (- 1 (hash-ref (hash-ref x 'l) 'l_discount))))]) v)))))] [(string? (let ([g b]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (* (hash-ref (hash-ref x 'l) 'l_extendedprice) (- 1 (hash-ref (hash-ref x 'l) 'l_discount))))]) v))))) (string>? (let ([g a]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (* (hash-ref (hash-ref x 'l) 'l_extendedprice) (- 1 (hash-ref (hash-ref x 'l) 'l_discount))))]) v)))) (let ([g b]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (* (hash-ref (hash-ref x 'l) 'l_extendedprice) (- 1 (hash-ref (hash-ref x 'l) 'l_discount))))]) v)))))] [else (> (let ([g a]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (* (hash-ref (hash-ref x 'l) 'l_extendedprice) (- 1 (hash-ref (hash-ref x 'l) 'l_discount))))]) v)))) (let ([g b]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (* (hash-ref (hash-ref x 'l) 'l_extendedprice) (- 1 (hash-ref (hash-ref x 'l) 'l_discount))))]) v)))))]))))
  (set! _groups (filter (lambda (g) (_gt (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'l) 'l_quantity))]) v)) threshold)) _groups))
  (for/list ([g _groups]) (hash 'c_name (hash-ref (hash-ref g 'key) 'c_name) 'c_custkey (hash-ref (hash-ref g 'key) 'c_custkey) 'revenue (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (* (hash-ref (hash-ref x 'l) 'l_extendedprice) (- 1 (hash-ref (hash-ref x 'l) 'l_discount))))]) v)) 'c_acctbal (hash-ref (hash-ref g 'key) 'c_acctbal) 'n_name (hash-ref (hash-ref g 'key) 'n_name) 'c_address (hash-ref (hash-ref g 'key) 'c_address) 'c_phone (hash-ref (hash-ref g 'key) 'c_phone) 'c_comment (hash-ref (hash-ref g 'key) 'c_comment)))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'c_name "Alice" 'c_custkey 1 'revenue 1700 'c_acctbal 1000 'n_name "GERMANY" 'c_address "123 Market St" 'c_phone "123-456" 'c_comment "Premium client"))) (displayln "ok"))
