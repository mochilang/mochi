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

(struct StoreSale (ss_item_sk ss_sold_date_sk ss_customer_sk ss_quantity ss_sales_price) #:transparent #:mutable)
(struct DateDim (d_date_sk d_year d_moy) #:transparent #:mutable)
(struct Item (i_item_sk) #:transparent #:mutable)
(struct CatalogSale (cs_sold_date_sk cs_item_sk cs_bill_customer_sk cs_quantity cs_list_price) #:transparent #:mutable)
(struct WebSale (ws_sold_date_sk ws_item_sk ws_bill_customer_sk ws_quantity ws_list_price) #:transparent #:mutable)
(define store_sales (list (hash 'ss_item_sk 1 'ss_sold_date_sk 1 'ss_customer_sk 1 'ss_quantity 1 'ss_sales_price 10) (hash 'ss_item_sk 1 'ss_sold_date_sk 1 'ss_customer_sk 1 'ss_quantity 1 'ss_sales_price 10) (hash 'ss_item_sk 1 'ss_sold_date_sk 1 'ss_customer_sk 1 'ss_quantity 1 'ss_sales_price 10) (hash 'ss_item_sk 1 'ss_sold_date_sk 1 'ss_customer_sk 1 'ss_quantity 1 'ss_sales_price 10) (hash 'ss_item_sk 1 'ss_sold_date_sk 1 'ss_customer_sk 1 'ss_quantity 1 'ss_sales_price 10) (hash 'ss_item_sk 2 'ss_sold_date_sk 1 'ss_customer_sk 2 'ss_quantity 1 'ss_sales_price 10) (hash 'ss_item_sk 2 'ss_sold_date_sk 1 'ss_customer_sk 2 'ss_quantity 1 'ss_sales_price 10) (hash 'ss_item_sk 2 'ss_sold_date_sk 1 'ss_customer_sk 2 'ss_quantity 1 'ss_sales_price 10)))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2000 'd_moy 1)))
(define item (list (hash 'i_item_sk 1) (hash 'i_item_sk 2)))
(define catalog_sales (list (hash 'cs_sold_date_sk 1 'cs_item_sk 1 'cs_bill_customer_sk 1 'cs_quantity 2 'cs_list_price 10) (hash 'cs_sold_date_sk 1 'cs_item_sk 2 'cs_bill_customer_sk 2 'cs_quantity 2 'cs_list_price 10)))
(define web_sales (list (hash 'ws_sold_date_sk 1 'ws_item_sk 1 'ws_bill_customer_sk 1 'ws_quantity 3 'ws_list_price 10) (hash 'ws_sold_date_sk 1 'ws_item_sk 2 'ws_bill_customer_sk 2 'ws_quantity 1 'ws_list_price 10)))
(define frequent_ss_items (let ([groups (make-hash)])
  (for* ([ss store_sales] [d date_dim] [i item] #:when (and (equal? (hash-ref ss 'ss_sold_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref ss 'ss_item_sk) (hash-ref i 'i_item_sk)) (equal? (hash-ref d 'd_year) 2000))) (let* ([key (hash 'item_sk (hash-ref i 'i_item_sk) 'date_sk (hash-ref d 'd_date_sk))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ss ss 'd d 'i i) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (filter (lambda (g) (_gt (length (hash-ref g 'items)) 4)) _groups))
  (for/list ([g _groups]) (hash-ref (hash-ref g 'key) 'item_sk))))
(define customer_totals (let ([groups (make-hash)])
  (for* ([ss store_sales]) (let* ([key (hash-ref ss 'ss_customer_sk)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons ss bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'cust (hash-ref g 'key) 'sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (* (hash-ref x 'ss_quantity) (hash-ref x 'ss_sales_price)))]) v))))))
(define max_sales (_max (for*/list ([c customer_totals]) (hash-ref c 'sales))))
(define best_ss_customer (for*/list ([c customer_totals] #:when (and (_gt (hash-ref c 'sales) (* 0.95 max_sales)))) (hash-ref c 'cust)))
(define catalog (for*/list ([cs catalog_sales] [d date_dim] #:when (and (equal? (hash-ref cs 'cs_sold_date_sk) (hash-ref d 'd_date_sk)) (and (and (and (equal? (hash-ref d 'd_year) 2000) (equal? (hash-ref d 'd_moy) 1)) (cond [(string? best_ss_customer) (regexp-match? (regexp (hash-ref cs 'cs_bill_customer_sk)) best_ss_customer)] [(hash? best_ss_customer) (hash-has-key? best_ss_customer (hash-ref cs 'cs_bill_customer_sk))] [else (member (hash-ref cs 'cs_bill_customer_sk) best_ss_customer)])) (cond [(string? frequent_ss_items) (regexp-match? (regexp (hash-ref cs 'cs_item_sk)) frequent_ss_items)] [(hash? frequent_ss_items) (hash-has-key? frequent_ss_items (hash-ref cs 'cs_item_sk))] [else (member (hash-ref cs 'cs_item_sk) frequent_ss_items)])))) (* (hash-ref cs 'cs_quantity) (hash-ref cs 'cs_list_price))))
(define web (for*/list ([ws web_sales] [d date_dim] #:when (and (equal? (hash-ref ws 'ws_sold_date_sk) (hash-ref d 'd_date_sk)) (and (and (and (equal? (hash-ref d 'd_year) 2000) (equal? (hash-ref d 'd_moy) 1)) (cond [(string? best_ss_customer) (regexp-match? (regexp (hash-ref ws 'ws_bill_customer_sk)) best_ss_customer)] [(hash? best_ss_customer) (hash-has-key? best_ss_customer (hash-ref ws 'ws_bill_customer_sk))] [else (member (hash-ref ws 'ws_bill_customer_sk) best_ss_customer)])) (cond [(string? frequent_ss_items) (regexp-match? (regexp (hash-ref ws 'ws_item_sk)) frequent_ss_items)] [(hash? frequent_ss_items) (hash-has-key? frequent_ss_items (hash-ref ws 'ws_item_sk))] [else (member (hash-ref ws 'ws_item_sk) frequent_ss_items)])))) (* (hash-ref ws 'ws_quantity) (hash-ref ws 'ws_list_price))))
(define result (+ (apply + (for*/list ([v catalog]) v)) (apply + (for*/list ([v web]) v))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result 50) (displayln "ok"))
