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

(struct StoreSale (ss_sold_date_sk ss_item_sk ss_customer_sk ss_ticket_number ss_quantity ss_store_sk) #:transparent #:mutable)
(struct StoreReturn (sr_returned_date_sk sr_customer_sk sr_item_sk sr_ticket_number sr_return_quantity) #:transparent #:mutable)
(struct CatalogSale (cs_sold_date_sk cs_item_sk cs_bill_customer_sk cs_quantity) #:transparent #:mutable)
(struct DateDim (d_date_sk d_quarter_name) #:transparent #:mutable)
(struct Store (s_store_sk s_state) #:transparent #:mutable)
(struct Item (i_item_sk i_item_id i_item_desc) #:transparent #:mutable)
(define store_sales (list (hash 'ss_sold_date_sk 1 'ss_item_sk 1 'ss_customer_sk 1 'ss_ticket_number 1 'ss_quantity 10 'ss_store_sk 1)))
(define store_returns (list (hash 'sr_returned_date_sk 2 'sr_customer_sk 1 'sr_item_sk 1 'sr_ticket_number 1 'sr_return_quantity 2)))
(define catalog_sales (list (hash 'cs_sold_date_sk 3 'cs_item_sk 1 'cs_bill_customer_sk 1 'cs_quantity 5)))
(define date_dim (list (hash 'd_date_sk 1 'd_quarter_name "1998Q1") (hash 'd_date_sk 2 'd_quarter_name "1998Q2") (hash 'd_date_sk 3 'd_quarter_name "1998Q3")))
(define store (list (hash 's_store_sk 1 's_state "CA")))
(define item (list (hash 'i_item_sk 1 'i_item_id "I1" 'i_item_desc "Item 1")))
(define joined (for*/list ([ss store_sales] [sr store_returns] [cs catalog_sales] [d1 date_dim] [d2 date_dim] [d3 date_dim] [s store] [i item] #:when (and (and (and (equal? (hash-ref ss 'ss_customer_sk) (hash-ref sr 'sr_customer_sk)) (equal? (hash-ref ss 'ss_item_sk) (hash-ref sr 'sr_item_sk))) (equal? (hash-ref ss 'ss_ticket_number) (hash-ref sr 'sr_ticket_number))) (and (equal? (hash-ref sr 'sr_customer_sk) (hash-ref cs 'cs_bill_customer_sk)) (equal? (hash-ref sr 'sr_item_sk) (hash-ref cs 'cs_item_sk))) (and (equal? (hash-ref ss 'ss_sold_date_sk) (hash-ref d1 'd_date_sk)) (string=? (hash-ref d1 'd_quarter_name) "1998Q1")) (and (equal? (hash-ref sr 'sr_returned_date_sk) (hash-ref d2 'd_date_sk)) (cond [(string? '("1998Q1" "1998Q2" "1998Q3")) (regexp-match? (regexp (hash-ref d2 'd_quarter_name)) '("1998Q1" "1998Q2" "1998Q3"))] [(hash? '("1998Q1" "1998Q2" "1998Q3")) (hash-has-key? '("1998Q1" "1998Q2" "1998Q3") (hash-ref d2 'd_quarter_name))] [else (member (hash-ref d2 'd_quarter_name) '("1998Q1" "1998Q2" "1998Q3"))])) (and (equal? (hash-ref cs 'cs_sold_date_sk) (hash-ref d3 'd_date_sk)) (cond [(string? '("1998Q1" "1998Q2" "1998Q3")) (regexp-match? (regexp (hash-ref d3 'd_quarter_name)) '("1998Q1" "1998Q2" "1998Q3"))] [(hash? '("1998Q1" "1998Q2" "1998Q3")) (hash-has-key? '("1998Q1" "1998Q2" "1998Q3") (hash-ref d3 'd_quarter_name))] [else (member (hash-ref d3 'd_quarter_name) '("1998Q1" "1998Q2" "1998Q3"))])) (equal? (hash-ref ss 'ss_store_sk) (hash-ref s 's_store_sk)) (equal? (hash-ref ss 'ss_item_sk) (hash-ref i 'i_item_sk)))) (hash 'qty (hash-ref ss 'ss_quantity) 'ret (hash-ref sr 'sr_return_quantity) 'csq (hash-ref cs 'cs_quantity) 'i_item_id (hash-ref i 'i_item_id) 'i_item_desc (hash-ref i 'i_item_desc) 's_state (hash-ref s 's_state))))
(define result (let ([groups (make-hash)])
  (for* ([j joined]) (let* ([key (hash 'i_item_id (hash-ref j 'i_item_id) 'i_item_desc (hash-ref j 'i_item_desc) 's_state (hash-ref j 's_state))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons j bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'i_item_id (hash-ref (hash-ref g 'key) 'i_item_id) 'i_item_desc (hash-ref (hash-ref g 'key) 'i_item_desc) 's_state (hash-ref (hash-ref g 'key) 's_state) 'store_sales_quantitycount (if (and (hash? (for*/list ([_ (hash-ref g 'items)]) _)) (hash-has-key? (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (hash-ref (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (for*/list ([_ (hash-ref g 'items)]) _))) 'store_sales_quantityave (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'qty))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'qty)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'store_sales_quantitystdev 0 'store_sales_quantitycov 0 'store_returns_quantitycount (if (and (hash? (for*/list ([_ (hash-ref g 'items)]) _)) (hash-has-key? (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (hash-ref (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (for*/list ([_ (hash-ref g 'items)]) _))) 'store_returns_quantityave (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ret))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ret)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'store_returns_quantitystdev 0 'store_returns_quantitycov 0 'catalog_sales_quantitycount (if (and (hash? (for*/list ([_ (hash-ref g 'items)]) _)) (hash-has-key? (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (hash-ref (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (for*/list ([_ (hash-ref g 'items)]) _))) 'catalog_sales_quantityave (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'csq))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'csq)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'catalog_sales_quantitystdev 0 'catalog_sales_quantitycov 0))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_item_id "I1" 'i_item_desc "Item 1" 's_state "CA" 'store_sales_quantitycount 1 'store_sales_quantityave 10 'store_sales_quantitystdev 0 'store_sales_quantitycov 0 'store_returns_quantitycount 1 'store_returns_quantityave 2 'store_returns_quantitystdev 0 'store_returns_quantitycov 0 'catalog_sales_quantitycount 1 'catalog_sales_quantityave 5 'catalog_sales_quantitystdev 0 'catalog_sales_quantitycov 0))) (displayln "ok"))
