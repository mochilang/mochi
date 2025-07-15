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

(define date_dim (list (hash 'd_date_sk 1 'd_year 2000) (hash 'd_date_sk 2 'd_year 2001)))
(define store_sales (list (hash 'ss_item_sk 1 'ss_quantity 50 'ss_sales_price 500 'ss_sold_date_sk 1) (hash 'ss_item_sk 1 'ss_quantity 40 'ss_sales_price 400 'ss_sold_date_sk 2)))
(define web_sales (list (hash 'ws_item_sk 1 'ws_quantity 30 'ws_sales_price 300 'ws_sold_date_sk 1) (hash 'ws_item_sk 1 'ws_quantity 25 'ws_sales_price 250 'ws_sold_date_sk 2)))
(define catalog_sales (list (hash 'cs_item_sk 1 'cs_quantity 20 'cs_sales_price 200 'cs_sold_date_sk 1) (hash 'cs_item_sk 1 'cs_quantity 15 'cs_sales_price 150 'cs_sold_date_sk 2)))
(define item (list (hash 'i_item_sk 1 'i_brand_id 1 'i_class_id 2 'i_category_id 3 'i_manufact_id 4 'i_category "Electronics")))
(define sales_detail (concat (for*/list ([ss store_sales] [d date_dim] #:when (and (equal? (hash-ref d 'd_date_sk) (hash-ref ss 'ss_sold_date_sk)))) (hash 'd_year (hash-ref d 'd_year) 'i_item_sk (hash-ref ss 'ss_item_sk) 'quantity (hash-ref ss 'ss_quantity) 'amount (hash-ref ss 'ss_sales_price))) (for*/list ([ws web_sales] [d date_dim] #:when (and (equal? (hash-ref d 'd_date_sk) (hash-ref ws 'ws_sold_date_sk)))) (hash 'd_year (hash-ref d 'd_year) 'i_item_sk (hash-ref ws 'ws_item_sk) 'quantity (hash-ref ws 'ws_quantity) 'amount (hash-ref ws 'ws_sales_price))) (for*/list ([cs catalog_sales] [d date_dim] #:when (and (equal? (hash-ref d 'd_date_sk) (hash-ref cs 'cs_sold_date_sk)))) (hash 'd_year (hash-ref d 'd_year) 'i_item_sk (hash-ref cs 'cs_item_sk) 'quantity (hash-ref cs 'cs_quantity) 'amount (hash-ref cs 'cs_sales_price)))))
(define all_sales (let ([groups (make-hash)])
  (for* ([sd sales_detail] [i item] #:when (and (equal? (hash-ref i 'i_item_sk) (hash-ref sd 'i_item_sk)) (string=? (hash-ref i 'i_category) "Electronics"))) (let* ([key (hash 'year (hash-ref sd 'd_year) 'brand_id (hash-ref i 'i_brand_id) 'class_id (hash-ref i 'i_class_id) 'category_id (hash-ref i 'i_category_id) 'manuf_id (hash-ref i 'i_manufact_id))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'sd sd 'i i) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'd_year (hash-ref (hash-ref g 'key) 'year) 'i_brand_id (hash-ref (hash-ref g 'key) 'brand_id) 'i_class_id (hash-ref (hash-ref g 'key) 'class_id) 'i_category_id (hash-ref (hash-ref g 'key) 'category_id) 'i_manufact_id (hash-ref (hash-ref g 'key) 'manuf_id) 'sales_cnt (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'sd) 'quantity))]) v)) 'sales_amt (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'sd) 'amount))]) v))))))
(define prev_yr (first (for*/list ([a all_sales] #:when (and (equal? (hash-ref a 'd_year) 2000))) a)))
(define curr_yr (first (for*/list ([a all_sales] #:when (and (equal? (hash-ref a 'd_year) 2001))) a)))
(define result (if (_lt (/ (hash-ref curr_yr 'sales_cnt) (hash-ref prev_yr 'sales_cnt)) 0.9) (list (hash 'prev_year (hash-ref prev_yr 'd_year) 'year (hash-ref curr_yr 'd_year) 'i_brand_id (hash-ref curr_yr 'i_brand_id) 'i_class_id (hash-ref curr_yr 'i_class_id) 'i_category_id (hash-ref curr_yr 'i_category_id) 'i_manufact_id (hash-ref curr_yr 'i_manufact_id) 'prev_yr_cnt (hash-ref prev_yr 'sales_cnt) 'curr_yr_cnt (hash-ref curr_yr 'sales_cnt) 'sales_cnt_diff (- (hash-ref curr_yr 'sales_cnt) (hash-ref prev_yr 'sales_cnt)) 'sales_amt_diff (- (hash-ref curr_yr 'sales_amt) (hash-ref prev_yr 'sales_amt)))) '()))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'prev_year 2000 'year 2001 'i_brand_id 1 'i_class_id 2 'i_category_id 3 'i_manufact_id 4 'prev_yr_cnt 100 'curr_yr_cnt 80 'sales_cnt_diff (- 20) 'sales_amt_diff (- 200)))) (displayln "ok"))
