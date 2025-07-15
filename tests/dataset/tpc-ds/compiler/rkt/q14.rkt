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

(struct StoreSale (ss_item_sk ss_list_price ss_quantity ss_sold_date_sk) #:transparent #:mutable)
(struct CatalogSale (cs_item_sk cs_list_price cs_quantity cs_sold_date_sk) #:transparent #:mutable)
(struct WebSale (ws_item_sk ws_list_price ws_quantity ws_sold_date_sk) #:transparent #:mutable)
(struct Item (i_item_sk i_brand_id i_class_id i_category_id) #:transparent #:mutable)
(struct DateDim (d_date_sk d_year d_moy) #:transparent #:mutable)
(define store_sales (list (hash 'ss_item_sk 1 'ss_list_price 10 'ss_quantity 2 'ss_sold_date_sk 1) (hash 'ss_item_sk 1 'ss_list_price 20 'ss_quantity 3 'ss_sold_date_sk 2)))
(define catalog_sales (list (hash 'cs_item_sk 1 'cs_list_price 10 'cs_quantity 2 'cs_sold_date_sk 1)))
(define web_sales (list (hash 'ws_item_sk 1 'ws_list_price 30 'ws_quantity 1 'ws_sold_date_sk 1)))
(define item (list (hash 'i_item_sk 1 'i_brand_id 1 'i_class_id 1 'i_category_id 1)))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2000 'd_moy 12) (hash 'd_date_sk 2 'd_year 2002 'd_moy 11)))
(define cross_items (list (hash 'ss_item_sk 1)))
(define avg_sales (let ([xs '(20 20 30)] [n (length '(20 20 30))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))))
(define store_filtered (let ([groups (make-hash)])
  (for* ([ss store_sales] [d date_dim] #:when (and (and (and (equal? (hash-ref ss 'ss_sold_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref d 'd_year) 2002)) (equal? (hash-ref d 'd_moy) 11)) (cond [(string? (for*/list ([ci cross_items]) (hash-ref ci 'ss_item_sk))) (regexp-match? (regexp (hash-ref ss 'ss_item_sk)) (for*/list ([ci cross_items]) (hash-ref ci 'ss_item_sk)))] [(hash? (for*/list ([ci cross_items]) (hash-ref ci 'ss_item_sk))) (hash-has-key? (for*/list ([ci cross_items]) (hash-ref ci 'ss_item_sk)) (hash-ref ss 'ss_item_sk))] [else (member (hash-ref ss 'ss_item_sk) (for*/list ([ci cross_items]) (hash-ref ci 'ss_item_sk)))]))) (let* ([key (hash 'brand_id 1 'class_id 1 'category_id 1)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ss ss 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'channel "store" 'sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (* (hash-ref x 'ss_quantity) (hash-ref x 'ss_list_price)))]) v)) 'number_sales (if (and (hash? (for*/list ([_ (hash-ref g 'items)]) _)) (hash-has-key? (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (hash-ref (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (for*/list ([_ (hash-ref g 'items)]) _)))))))
(define result (for*/list ([r store_filtered] #:when (and (_gt (hash-ref r 'sales) avg_sales))) (hash 'channel (hash-ref r 'channel) 'i_brand_id 1 'i_class_id 1 'i_category_id 1 'sales (hash-ref r 'sales) 'number_sales (hash-ref r 'number_sales))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'channel "store" 'i_brand_id 1 'i_class_id 1 'i_category_id 1 'sales 60 'number_sales 1))) (displayln "ok"))
