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

(define store_sales (list (hash 'cdemo_sk 1 'addr_sk 1 'sold_date_sk 1 'sales_price 120 'net_profit 1000 'quantity 5) (hash 'cdemo_sk 2 'addr_sk 2 'sold_date_sk 1 'sales_price 60 'net_profit 2000 'quantity 10) (hash 'cdemo_sk 3 'addr_sk 3 'sold_date_sk 1 'sales_price 170 'net_profit 10000 'quantity 20)))
(define store (list (hash 's_store_sk 1)))
(define customer_demographics (list (hash 'cd_demo_sk 1 'cd_marital_status "S" 'cd_education_status "E1") (hash 'cd_demo_sk 2 'cd_marital_status "M" 'cd_education_status "E2") (hash 'cd_demo_sk 3 'cd_marital_status "W" 'cd_education_status "E3")))
(define customer_address (list (hash 'ca_address_sk 1 'ca_country "United States" 'ca_state "TX") (hash 'ca_address_sk 2 'ca_country "United States" 'ca_state "CA") (hash 'ca_address_sk 3 'ca_country "United States" 'ca_state "NY")))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2000)))
(define year 2000)
(define states1 '("TX"))
(define states2 '("CA"))
(define states3 '("NY"))
(define qty_base (for*/list ([ss store_sales] [cd customer_demographics] [ca customer_address] [d date_dim] #:when (and (equal? (hash-ref ss 'cdemo_sk) (hash-ref cd 'cd_demo_sk)) (equal? (hash-ref ss 'addr_sk) (hash-ref ca 'ca_address_sk)) (equal? (hash-ref ss 'sold_date_sk) (hash-ref d 'd_date_sk)) (and (and (equal? (hash-ref d 'd_year) year) (or (or (and (and (and (string=? (hash-ref cd 'cd_marital_status) "S") (string=? (hash-ref cd 'cd_education_status) "E1")) (_ge (hash-ref ss 'sales_price) 100)) (_le (hash-ref ss 'sales_price) 150)) (and (and (and (string=? (hash-ref cd 'cd_marital_status) "M") (string=? (hash-ref cd 'cd_education_status) "E2")) (_ge (hash-ref ss 'sales_price) 50)) (_le (hash-ref ss 'sales_price) 100))) (and (and (and (string=? (hash-ref cd 'cd_marital_status) "W") (string=? (hash-ref cd 'cd_education_status) "E3")) (_ge (hash-ref ss 'sales_price) 150)) (_le (hash-ref ss 'sales_price) 200)))) (or (or (and (and (cond [(string? states1) (regexp-match? (regexp (hash-ref ca 'ca_state)) states1)] [(hash? states1) (hash-has-key? states1 (hash-ref ca 'ca_state))] [else (member (hash-ref ca 'ca_state) states1)]) (_ge (hash-ref ss 'net_profit) 0)) (_le (hash-ref ss 'net_profit) 2000)) (and (and (cond [(string? states2) (regexp-match? (regexp (hash-ref ca 'ca_state)) states2)] [(hash? states2) (hash-has-key? states2 (hash-ref ca 'ca_state))] [else (member (hash-ref ca 'ca_state) states2)]) (_ge (hash-ref ss 'net_profit) 150)) (_le (hash-ref ss 'net_profit) 3000))) (and (and (cond [(string? states3) (regexp-match? (regexp (hash-ref ca 'ca_state)) states3)] [(hash? states3) (hash-has-key? states3 (hash-ref ca 'ca_state))] [else (member (hash-ref ca 'ca_state) states3)]) (_ge (hash-ref ss 'net_profit) 50)) (_le (hash-ref ss 'net_profit) 25000)))))) (hash-ref ss 'quantity)))
(define qty qty_base)
(define result (apply + (for*/list ([v qty]) v)))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result 35) (displayln "ok"))
