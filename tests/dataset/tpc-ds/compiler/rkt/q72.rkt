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

(define catalog_sales (list (hash 'cs_item_sk 1 'cs_order_number 1 'cs_quantity 1 'cs_sold_date_sk 1 'cs_ship_date_sk 3 'cs_bill_cdemo_sk 1 'cs_bill_hdemo_sk 1 'cs_promo_sk '())))
(define inventory (list (hash 'inv_item_sk 1 'inv_warehouse_sk 1 'inv_date_sk 2 'inv_quantity_on_hand 0)))
(define warehouse (list (hash 'w_warehouse_sk 1 'w_warehouse_name "Main")))
(define item (list (hash 'i_item_sk 1 'i_item_desc "ItemA")))
(define customer_demographics (list (hash 'cd_demo_sk 1 'cd_marital_status "M")))
(define household_demographics (list (hash 'hd_demo_sk 1 'hd_buy_potential "5001-10000")))
(define date_dim (list (hash 'd_date_sk 1 'd_week_seq 10 'd_date 1 'd_year 2000) (hash 'd_date_sk 2 'd_week_seq 10 'd_date 1 'd_year 2000) (hash 'd_date_sk 3 'd_week_seq 10 'd_date 7 'd_year 2000)))
(define result (let ([groups (make-hash)])
  (for* ([cs catalog_sales] [inv inventory] [w warehouse] [i item] [cd customer_demographics] [hd household_demographics] [d1 date_dim] [d2 date_dim] [d3 date_dim] #:when (and (equal? (hash-ref inv 'inv_item_sk) (hash-ref cs 'cs_item_sk)) (equal? (hash-ref w 'w_warehouse_sk) (hash-ref inv 'inv_warehouse_sk)) (equal? (hash-ref i 'i_item_sk) (hash-ref cs 'cs_item_sk)) (equal? (hash-ref cd 'cd_demo_sk) (hash-ref cs 'cs_bill_cdemo_sk)) (equal? (hash-ref hd 'hd_demo_sk) (hash-ref cs 'cs_bill_hdemo_sk)) (equal? (hash-ref d1 'd_date_sk) (hash-ref cs 'cs_sold_date_sk)) (equal? (hash-ref d2 'd_date_sk) (hash-ref inv 'inv_date_sk)) (equal? (hash-ref d3 'd_date_sk) (hash-ref cs 'cs_ship_date_sk)) (and (and (and (and (and (equal? (hash-ref d1 'd_week_seq) (hash-ref d2 'd_week_seq)) (_lt (hash-ref inv 'inv_quantity_on_hand) (hash-ref cs 'cs_quantity))) (_gt (hash-ref d3 'd_date) (+ (hash-ref d1 'd_date) 5))) (string=? (hash-ref hd 'hd_buy_potential) "5001-10000")) (equal? (hash-ref d1 'd_year) 2000)) (string=? (hash-ref cd 'cd_marital_status) "M")))) (let* ([key (hash 'item_desc (hash-ref i 'i_item_desc) 'warehouse (hash-ref w 'w_warehouse_name) 'week_seq (hash-ref d1 'd_week_seq))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'cs cs 'inv inv 'w w 'i i 'cd cd 'hd hd 'd1 d1 'd2 d2 'd3 d3) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'i_item_desc (hash-ref (hash-ref g 'key) 'item_desc) 'w_warehouse_name (hash-ref (hash-ref g 'key) 'warehouse) 'd_week_seq (hash-ref (hash-ref g 'key) 'week_seq) 'no_promo (if (and (hash? (for*/list ([x (hash-ref g 'items)] #:when (and (equal? (hash-ref x 'cs_promo_sk) '()))) x)) (hash-has-key? (for*/list ([x (hash-ref g 'items)] #:when (and (equal? (hash-ref x 'cs_promo_sk) '()))) x) 'items)) (length (hash-ref (for*/list ([x (hash-ref g 'items)] #:when (and (equal? (hash-ref x 'cs_promo_sk) '()))) x) 'items)) (length (for*/list ([x (hash-ref g 'items)] #:when (and (equal? (hash-ref x 'cs_promo_sk) '()))) x))) 'promo (if (and (hash? (for*/list ([x (hash-ref g 'items)] #:when (and (not (equal? (hash-ref x 'cs_promo_sk) '())))) x)) (hash-has-key? (for*/list ([x (hash-ref g 'items)] #:when (and (not (equal? (hash-ref x 'cs_promo_sk) '())))) x) 'items)) (length (hash-ref (for*/list ([x (hash-ref g 'items)] #:when (and (not (equal? (hash-ref x 'cs_promo_sk) '())))) x) 'items)) (length (for*/list ([x (hash-ref g 'items)] #:when (and (not (equal? (hash-ref x 'cs_promo_sk) '())))) x))) 'total_cnt (length (hash-ref g 'items))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_item_desc "ItemA" 'w_warehouse_name "Main" 'd_week_seq 10 'no_promo 1 'promo 0 'total_cnt 1))) (displayln "ok"))
