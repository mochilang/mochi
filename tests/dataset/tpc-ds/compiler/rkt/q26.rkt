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

(struct CatalogSale (cs_sold_date_sk cs_item_sk cs_bill_cdemo_sk cs_promo_sk cs_quantity cs_list_price cs_coupon_amt cs_sales_price) #:transparent #:mutable)
(struct CustomerDemo (cd_demo_sk cd_gender cd_marital_status cd_education_status) #:transparent #:mutable)
(struct DateDim (d_date_sk d_year) #:transparent #:mutable)
(struct Item (i_item_sk i_item_id) #:transparent #:mutable)
(struct Promotion (p_promo_sk p_channel_email p_channel_event) #:transparent #:mutable)
(define catalog_sales (list (hash 'cs_sold_date_sk 1 'cs_item_sk 1 'cs_bill_cdemo_sk 1 'cs_promo_sk 1 'cs_quantity 10 'cs_list_price 100 'cs_coupon_amt 5 'cs_sales_price 95) (hash 'cs_sold_date_sk 1 'cs_item_sk 2 'cs_bill_cdemo_sk 2 'cs_promo_sk 2 'cs_quantity 5 'cs_list_price 50 'cs_coupon_amt 2 'cs_sales_price 48)))
(define customer_demographics (list (hash 'cd_demo_sk 1 'cd_gender "M" 'cd_marital_status "S" 'cd_education_status "College") (hash 'cd_demo_sk 2 'cd_gender "F" 'cd_marital_status "M" 'cd_education_status "High School")))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2000)))
(define item (list (hash 'i_item_sk 1 'i_item_id "ITEM1") (hash 'i_item_sk 2 'i_item_id "ITEM2")))
(define promotion (list (hash 'p_promo_sk 1 'p_channel_email "N" 'p_channel_event "Y") (hash 'p_promo_sk 2 'p_channel_email "Y" 'p_channel_event "N")))
(define result (let ([groups (make-hash)])
  (for* ([cs catalog_sales] [cd customer_demographics] [d date_dim] [i item] [p promotion] #:when (and (equal? (hash-ref cs 'cs_bill_cdemo_sk) (hash-ref cd 'cd_demo_sk)) (equal? (hash-ref cs 'cs_sold_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref cs 'cs_item_sk) (hash-ref i 'i_item_sk)) (equal? (hash-ref cs 'cs_promo_sk) (hash-ref p 'p_promo_sk)) (and (and (and (and (string=? (hash-ref cd 'cd_gender) "M") (string=? (hash-ref cd 'cd_marital_status) "S")) (string=? (hash-ref cd 'cd_education_status) "College")) (or (string=? (hash-ref p 'p_channel_email) "N") (string=? (hash-ref p 'p_channel_event) "N"))) (equal? (hash-ref d 'd_year) 2000)))) (let* ([key (hash-ref i 'i_item_id)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'cs cs 'cd cd 'd d 'i i 'p p) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'i_item_id (hash-ref g 'key) 'agg1 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_quantity))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_quantity)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg2 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_list_price))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_list_price)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg3 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_coupon_amt))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_coupon_amt)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg4 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_sales_price))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_sales_price)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_item_id "ITEM1" 'agg1 10 'agg2 100 'agg3 5 'agg4 95))) (displayln "ok"))
