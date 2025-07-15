#lang racket
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

(struct CatalogSale (cs_quantity cs_list_price cs_coupon_amt cs_sales_price cs_net_profit cs_bill_cdemo_sk cs_bill_customer_sk cs_sold_date_sk cs_item_sk) #:transparent #:mutable)
(struct CustomerDemographics (cd_demo_sk cd_gender cd_education_status cd_dep_count) #:transparent #:mutable)
(struct Customer (c_customer_sk c_current_cdemo_sk c_current_addr_sk c_birth_year c_birth_month) #:transparent #:mutable)
(struct CustomerAddress (ca_address_sk ca_country ca_state ca_county) #:transparent #:mutable)
(struct DateDim (d_date_sk d_year) #:transparent #:mutable)
(struct Item (i_item_sk i_item_id) #:transparent #:mutable)
(define catalog_sales (list (hash 'cs_quantity 1 'cs_list_price 10 'cs_coupon_amt 1 'cs_sales_price 9 'cs_net_profit 2 'cs_bill_cdemo_sk 1 'cs_bill_customer_sk 1 'cs_sold_date_sk 1 'cs_item_sk 1)))
(define customer_demographics (list (hash 'cd_demo_sk 1 'cd_gender "M" 'cd_education_status "College" 'cd_dep_count 2) (hash 'cd_demo_sk 2 'cd_gender "F" 'cd_education_status "College" 'cd_dep_count 2)))
(define customer (list (hash 'c_customer_sk 1 'c_current_cdemo_sk 2 'c_current_addr_sk 1 'c_birth_year 1980 'c_birth_month 1)))
(define customer_address (list (hash 'ca_address_sk 1 'ca_country "US" 'ca_state "CA" 'ca_county "County1")))
(define date_dim (list (hash 'd_date_sk 1 'd_year 1999)))
(define item (list (hash 'i_item_sk 1 'i_item_id "I1")))
(define joined (for*/list ([cs catalog_sales] [cd1 customer_demographics] [c customer] [cd2 customer_demographics] [ca customer_address] [d date_dim] [i item] #:when (and (and (and (equal? (hash-ref cs 'cs_bill_cdemo_sk) (hash-ref cd1 'cd_demo_sk)) (string=? (hash-ref cd1 'cd_gender) "M")) (string=? (hash-ref cd1 'cd_education_status) "College")) (equal? (hash-ref cs 'cs_bill_customer_sk) (hash-ref c 'c_customer_sk)) (equal? (hash-ref c 'c_current_cdemo_sk) (hash-ref cd2 'cd_demo_sk)) (equal? (hash-ref c 'c_current_addr_sk) (hash-ref ca 'ca_address_sk)) (and (equal? (hash-ref cs 'cs_sold_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref d 'd_year) 1999)) (equal? (hash-ref cs 'cs_item_sk) (hash-ref i 'i_item_sk)))) (hash 'i_item_id (hash-ref i 'i_item_id) 'ca_country (hash-ref ca 'ca_country) 'ca_state (hash-ref ca 'ca_state) 'ca_county (hash-ref ca 'ca_county) 'q (hash-ref cs 'cs_quantity) 'lp (hash-ref cs 'cs_list_price) 'cp (hash-ref cs 'cs_coupon_amt) 'sp (hash-ref cs 'cs_sales_price) 'np (hash-ref cs 'cs_net_profit) 'by (hash-ref c 'c_birth_year) 'dep (hash-ref cd1 'cd_dep_count))))
(define result (let ([groups (make-hash)])
  (for* ([j joined]) (let* ([key (hash 'i_item_id (hash-ref j 'i_item_id) 'ca_country (hash-ref j 'ca_country) 'ca_state (hash-ref j 'ca_state) 'ca_county (hash-ref j 'ca_county))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons j bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'i_item_id (hash-ref (hash-ref g 'key) 'i_item_id) 'ca_country (hash-ref (hash-ref g 'key) 'ca_country) 'ca_state (hash-ref (hash-ref g 'key) 'ca_state) 'ca_county (hash-ref (hash-ref g 'key) 'ca_county) 'agg1 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'q))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'q)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg2 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'lp))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'lp)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg3 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cp))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cp)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg4 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'sp))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'sp)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg5 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'np))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'np)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg6 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'by))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'by)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg7 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'dep))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'dep)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_item_id "I1" 'ca_country "US" 'ca_state "CA" 'ca_county "County1" 'agg1 1 'agg2 10 'agg3 1 'agg4 9 'agg5 2 'agg6 1980 'agg7 2))) (displayln "ok"))
