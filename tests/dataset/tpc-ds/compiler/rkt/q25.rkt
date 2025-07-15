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

(struct StoreSale (ss_sold_date_sk ss_item_sk ss_store_sk ss_customer_sk ss_net_profit ss_ticket_number) #:transparent #:mutable)
(struct StoreReturn (sr_returned_date_sk sr_item_sk sr_customer_sk sr_ticket_number sr_net_loss) #:transparent #:mutable)
(struct CatalogSale (cs_sold_date_sk cs_item_sk cs_bill_customer_sk cs_net_profit) #:transparent #:mutable)
(struct DateDim (d_date_sk d_moy d_year) #:transparent #:mutable)
(struct Store (s_store_sk s_store_id s_store_name) #:transparent #:mutable)
(struct Item (i_item_sk i_item_id i_item_desc) #:transparent #:mutable)
(define store_sales (list (hash 'ss_sold_date_sk 1 'ss_item_sk 1 'ss_store_sk 1 'ss_customer_sk 1 'ss_net_profit 50 'ss_ticket_number 1) (hash 'ss_sold_date_sk 1 'ss_item_sk 2 'ss_store_sk 1 'ss_customer_sk 2 'ss_net_profit 20 'ss_ticket_number 2)))
(define store_returns (list (hash 'sr_returned_date_sk 2 'sr_item_sk 1 'sr_customer_sk 1 'sr_ticket_number 1 'sr_net_loss 10) (hash 'sr_returned_date_sk 2 'sr_item_sk 2 'sr_customer_sk 2 'sr_ticket_number 2 'sr_net_loss 5)))
(define catalog_sales (list (hash 'cs_sold_date_sk 3 'cs_item_sk 1 'cs_bill_customer_sk 1 'cs_net_profit 30) (hash 'cs_sold_date_sk 3 'cs_item_sk 2 'cs_bill_customer_sk 2 'cs_net_profit 15)))
(define date_dim (list (hash 'd_date_sk 1 'd_moy 4 'd_year 2000) (hash 'd_date_sk 2 'd_moy 5 'd_year 2000) (hash 'd_date_sk 3 'd_moy 6 'd_year 2000)))
(define store (list (hash 's_store_sk 1 's_store_id "S1" 's_store_name "Store1")))
(define item (list (hash 'i_item_sk 1 'i_item_id "ITEM1" 'i_item_desc "Desc1") (hash 'i_item_sk 2 'i_item_id "ITEM2" 'i_item_desc "Desc2")))
(define result (let ([groups (make-hash)])
  (for* ([ss store_sales] [sr store_returns] [cs catalog_sales] [d1 date_dim] [d2 date_dim] [d3 date_dim] [s store] [i item] #:when (and (and (equal? (hash-ref ss 'ss_ticket_number) (hash-ref sr 'sr_ticket_number)) (equal? (hash-ref ss 'ss_item_sk) (hash-ref sr 'sr_item_sk))) (and (equal? (hash-ref sr 'sr_customer_sk) (hash-ref cs 'cs_bill_customer_sk)) (equal? (hash-ref sr 'sr_item_sk) (hash-ref cs 'cs_item_sk))) (equal? (hash-ref d1 'd_date_sk) (hash-ref ss 'ss_sold_date_sk)) (equal? (hash-ref d2 'd_date_sk) (hash-ref sr 'sr_returned_date_sk)) (equal? (hash-ref d3 'd_date_sk) (hash-ref cs 'cs_sold_date_sk)) (equal? (hash-ref s 's_store_sk) (hash-ref ss 'ss_store_sk)) (equal? (hash-ref i 'i_item_sk) (hash-ref ss 'ss_item_sk)) (and (and (and (and (and (equal? (hash-ref d1 'd_moy) 4) (equal? (hash-ref d1 'd_year) 2000)) (_ge (hash-ref d2 'd_moy) 4)) (_le (hash-ref d2 'd_moy) 10)) (_ge (hash-ref d3 'd_moy) 4)) (_le (hash-ref d3 'd_moy) 10)))) (let* ([key (hash 'item_id (hash-ref i 'i_item_id) 'item_desc (hash-ref i 'i_item_desc) 's_store_id (hash-ref s 's_store_id) 's_store_name (hash-ref s 's_store_name))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ss ss 'sr sr 'cs cs 'd1 d1 'd2 d2 'd3 d3 's s 'i i) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'i_item_id (hash-ref (hash-ref g 'key) 'item_id) 'i_item_desc (hash-ref (hash-ref g 'key) 'item_desc) 's_store_id (hash-ref (hash-ref g 'key) 's_store_id) 's_store_name (hash-ref (hash-ref g 'key) 's_store_name) 'store_sales_profit (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_net_profit))]) v)) 'store_returns_loss (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'sr_net_loss))]) v)) 'catalog_sales_profit (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_net_profit))]) v))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_item_id "ITEM1" 'i_item_desc "Desc1" 's_store_id "S1" 's_store_name "Store1" 'store_sales_profit 50 'store_returns_loss 10 'catalog_sales_profit 30) (hash 'i_item_id "ITEM2" 'i_item_desc "Desc2" 's_store_id "S1" 's_store_name "Store1" 'store_sales_profit 20 'store_returns_loss 5 'catalog_sales_profit 15))) (displayln "ok"))
