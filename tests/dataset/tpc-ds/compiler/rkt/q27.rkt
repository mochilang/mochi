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

(struct StoreSale (ss_item_sk ss_store_sk ss_cdemo_sk ss_sold_date_sk ss_quantity ss_list_price ss_coupon_amt ss_sales_price) #:transparent #:mutable)
(struct CustomerDemo (cd_demo_sk cd_gender cd_marital_status cd_education_status) #:transparent #:mutable)
(struct DateDim (d_date_sk d_year) #:transparent #:mutable)
(struct Store (s_store_sk s_state) #:transparent #:mutable)
(struct Item (i_item_sk i_item_id) #:transparent #:mutable)
(define store_sales (list (hash 'ss_item_sk 1 'ss_store_sk 1 'ss_cdemo_sk 1 'ss_sold_date_sk 1 'ss_quantity 5 'ss_list_price 100 'ss_coupon_amt 10 'ss_sales_price 90) (hash 'ss_item_sk 2 'ss_store_sk 2 'ss_cdemo_sk 2 'ss_sold_date_sk 1 'ss_quantity 2 'ss_list_price 50 'ss_coupon_amt 5 'ss_sales_price 45)))
(define customer_demographics (list (hash 'cd_demo_sk 1 'cd_gender "F" 'cd_marital_status "M" 'cd_education_status "College") (hash 'cd_demo_sk 2 'cd_gender "M" 'cd_marital_status "S" 'cd_education_status "College")))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2000)))
(define store (list (hash 's_store_sk 1 's_state "CA") (hash 's_store_sk 2 's_state "TX")))
(define item (list (hash 'i_item_sk 1 'i_item_id "ITEM1") (hash 'i_item_sk 2 'i_item_id "ITEM2")))
(define result (let ([groups (make-hash)])
  (for* ([ss store_sales] [cd customer_demographics] [d date_dim] [s store] [i item] #:when (and (equal? (hash-ref ss 'ss_cdemo_sk) (hash-ref cd 'cd_demo_sk)) (equal? (hash-ref ss 'ss_sold_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref ss 'ss_store_sk) (hash-ref s 's_store_sk)) (equal? (hash-ref ss 'ss_item_sk) (hash-ref i 'i_item_sk)) (and (and (and (and (string=? (hash-ref cd 'cd_gender) "F") (string=? (hash-ref cd 'cd_marital_status) "M")) (string=? (hash-ref cd 'cd_education_status) "College")) (equal? (hash-ref d 'd_year) 2000)) (cond [(string? '("CA")) (regexp-match? (regexp (hash-ref s 's_state)) '("CA"))] [(hash? '("CA")) (hash-has-key? '("CA") (hash-ref s 's_state))] [else (member (hash-ref s 's_state) '("CA"))])))) (let* ([key (hash 'item_id (hash-ref i 'i_item_id) 'state (hash-ref s 's_state))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ss ss 'cd cd 'd d 's s 'i i) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'item_id) (hash-ref (hash-ref g 'key) 'state)))) (string>? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'item_id) (hash-ref (hash-ref g 'key) 'state))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'item_id) (hash-ref (hash-ref g 'key) 'state))))] [(string? (let ([g b]) (list (hash-ref (hash-ref g 'key) 'item_id) (hash-ref (hash-ref g 'key) 'state)))) (string>? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'item_id) (hash-ref (hash-ref g 'key) 'state))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'item_id) (hash-ref (hash-ref g 'key) 'state))))] [else (> (let ([g a]) (list (hash-ref (hash-ref g 'key) 'item_id) (hash-ref (hash-ref g 'key) 'state))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'item_id) (hash-ref (hash-ref g 'key) 'state))))]))))
  (for/list ([g _groups]) (hash 'i_item_id (hash-ref (hash-ref g 'key) 'item_id) 's_state (hash-ref (hash-ref g 'key) 'state) 'agg1 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_quantity))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_quantity)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg2 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_list_price))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_list_price)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg3 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_coupon_amt))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_coupon_amt)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg4 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_sales_price))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_sales_price)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_item_id "ITEM1" 's_state "CA" 'agg1 5 'agg2 100 'agg3 10 'agg4 90))) (displayln "ok"))
