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

(struct StoreSale (ss_store_sk ss_sold_date_sk ss_hdemo_sk ss_cdemo_sk ss_addr_sk ss_sales_price ss_net_profit ss_quantity ss_ext_sales_price ss_ext_wholesale_cost) #:transparent #:mutable)
(struct Store (s_store_sk s_state) #:transparent #:mutable)
(struct CustomerDemographics (cd_demo_sk cd_marital_status cd_education_status) #:transparent #:mutable)
(struct HouseholdDemographics (hd_demo_sk hd_dep_count) #:transparent #:mutable)
(struct CustomerAddress (ca_address_sk ca_country ca_state) #:transparent #:mutable)
(struct DateDim (d_date_sk d_year) #:transparent #:mutable)
(define store_sales (list (hash 'ss_store_sk 1 'ss_sold_date_sk 1 'ss_hdemo_sk 1 'ss_cdemo_sk 1 'ss_addr_sk 1 'ss_sales_price 120 'ss_net_profit 150 'ss_quantity 10 'ss_ext_sales_price 100 'ss_ext_wholesale_cost 50)))
(define store (list (hash 's_store_sk 1 's_state "CA")))
(define customer_demographics (list (hash 'cd_demo_sk 1 'cd_marital_status "M1" 'cd_education_status "ES1")))
(define household_demographics (list (hash 'hd_demo_sk 1 'hd_dep_count 3)))
(define customer_address (list (hash 'ca_address_sk 1 'ca_country "United States" 'ca_state "CA")))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2001)))
(define filtered (for*/list ([ss store_sales] [s store] [cd customer_demographics] [hd household_demographics] [ca customer_address] [d date_dim] #:when (and (equal? (hash-ref ss 'ss_store_sk) (hash-ref s 's_store_sk)) (and (and (equal? (hash-ref ss 'ss_cdemo_sk) (hash-ref cd 'cd_demo_sk)) (string=? (hash-ref cd 'cd_marital_status) "M1")) (string=? (hash-ref cd 'cd_education_status) "ES1")) (and (equal? (hash-ref ss 'ss_hdemo_sk) (hash-ref hd 'hd_demo_sk)) (equal? (hash-ref hd 'hd_dep_count) 3)) (and (and (equal? (hash-ref ss 'ss_addr_sk) (hash-ref ca 'ca_address_sk)) (string=? (hash-ref ca 'ca_country) "United States")) (string=? (hash-ref ca 'ca_state) "CA")) (and (equal? (hash-ref ss 'ss_sold_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref d 'd_year) 2001)))) ss))
(define result (let ([groups (make-hash)])
  (for* ([r filtered]) (let* ([key (hash )] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons r bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'avg_ss_quantity (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_quantity))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_quantity)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'avg_ss_ext_sales_price (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_sales_price))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_sales_price)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'avg_ss_ext_wholesale_cost (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_wholesale_cost))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_wholesale_cost)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'sum_ss_ext_wholesale_cost (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_wholesale_cost))]) v))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'avg_ss_quantity 10 'avg_ss_ext_sales_price 100 'avg_ss_ext_wholesale_cost 50 'sum_ss_ext_wholesale_cost 50))) (displayln "ok"))
