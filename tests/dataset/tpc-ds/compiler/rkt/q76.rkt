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

(define date_dim (list (hash 'd_date_sk 1 'd_year 1998 'd_qoy 1)))
(define item (list (hash 'i_item_sk 1 'i_category "CatA") (hash 'i_item_sk 2 'i_category "CatB") (hash 'i_item_sk 3 'i_category "CatC")))
(define store_sales (list (hash 'ss_customer_sk '() 'ss_item_sk 1 'ss_ext_sales_price 10 'ss_sold_date_sk 1)))
(define web_sales (list (hash 'ws_bill_customer_sk '() 'ws_item_sk 2 'ws_ext_sales_price 15 'ws_sold_date_sk 1)))
(define catalog_sales (list (hash 'cs_bill_customer_sk '() 'cs_item_sk 3 'cs_ext_sales_price 20 'cs_sold_date_sk 1)))
(define store_part (for*/list ([ss store_sales] [i item] [d date_dim] #:when (and (equal? (hash-ref i 'i_item_sk) (hash-ref ss 'ss_item_sk)) (equal? (hash-ref d 'd_date_sk) (hash-ref ss 'ss_sold_date_sk)) (equal? (hash-ref ss 'ss_customer_sk) '()))) (hash 'channel "store" 'col_name (hash-ref ss 'ss_customer_sk) 'd_year (hash-ref d 'd_year) 'd_qoy (hash-ref d 'd_qoy) 'i_category (hash-ref i 'i_category) 'ext_sales_price (hash-ref ss 'ss_ext_sales_price))))
(define web_part (for*/list ([ws web_sales] [i item] [d date_dim] #:when (and (equal? (hash-ref i 'i_item_sk) (hash-ref ws 'ws_item_sk)) (equal? (hash-ref d 'd_date_sk) (hash-ref ws 'ws_sold_date_sk)) (equal? (hash-ref ws 'ws_bill_customer_sk) '()))) (hash 'channel "web" 'col_name (hash-ref ws 'ws_bill_customer_sk) 'd_year (hash-ref d 'd_year) 'd_qoy (hash-ref d 'd_qoy) 'i_category (hash-ref i 'i_category) 'ext_sales_price (hash-ref ws 'ws_ext_sales_price))))
(define catalog_part (for*/list ([cs catalog_sales] [i item] [d date_dim] #:when (and (equal? (hash-ref i 'i_item_sk) (hash-ref cs 'cs_item_sk)) (equal? (hash-ref d 'd_date_sk) (hash-ref cs 'cs_sold_date_sk)) (equal? (hash-ref cs 'cs_bill_customer_sk) '()))) (hash 'channel "catalog" 'col_name (hash-ref cs 'cs_bill_customer_sk) 'd_year (hash-ref d 'd_year) 'd_qoy (hash-ref d 'd_qoy) 'i_category (hash-ref i 'i_category) 'ext_sales_price (hash-ref cs 'cs_ext_sales_price))))
(define all_rows (concat store_part web_part catalog_part))
(define result (let ([groups (make-hash)])
  (for* ([r all_rows]) (let* ([key (hash 'channel (hash-ref r 'channel) 'col_name (hash-ref r 'col_name) 'd_year (hash-ref r 'd_year) 'd_qoy (hash-ref r 'd_qoy) 'i_category (hash-ref r 'i_category))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons r bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (hash-ref (hash-ref g 'key) 'channel))) (string>? (let ([g a]) (hash-ref (hash-ref g 'key) 'channel)) (let ([g b]) (hash-ref (hash-ref g 'key) 'channel)))] [(string? (let ([g b]) (hash-ref (hash-ref g 'key) 'channel))) (string>? (let ([g a]) (hash-ref (hash-ref g 'key) 'channel)) (let ([g b]) (hash-ref (hash-ref g 'key) 'channel)))] [else (> (let ([g a]) (hash-ref (hash-ref g 'key) 'channel)) (let ([g b]) (hash-ref (hash-ref g 'key) 'channel)))]))))
  (for/list ([g _groups]) (hash 'channel (hash-ref (hash-ref g 'key) 'channel) 'col_name (hash-ref (hash-ref g 'key) 'col_name) 'd_year (hash-ref (hash-ref g 'key) 'd_year) 'd_qoy (hash-ref (hash-ref g 'key) 'd_qoy) 'i_category (hash-ref (hash-ref g 'key) 'i_category) 'sales_cnt (length (hash-ref g 'items)) 'sales_amt (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'r) 'ext_sales_price))]) v))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'channel "store" 'col_name '() 'd_year 1998 'd_qoy 1 'i_category "CatA" 'sales_cnt 1 'sales_amt 10) (hash 'channel "web" 'col_name '() 'd_year 1998 'd_qoy 1 'i_category "CatB" 'sales_cnt 1 'sales_amt 15) (hash 'channel "catalog" 'col_name '() 'd_year 1998 'd_qoy 1 'i_category "CatC" 'sales_cnt 1 'sales_amt 20))) (displayln "ok"))
