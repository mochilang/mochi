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

(define item (list (hash 'i_item_sk 1 'i_manufact_id 1 'i_category "Books") (hash 'i_item_sk 2 'i_manufact_id 2 'i_category "Books")))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2000 'd_moy 1)))
(define customer_address (list (hash 'ca_address_sk 1 'ca_gmt_offset (- 5)) (hash 'ca_address_sk 2 'ca_gmt_offset (- 5))))
(define store_sales (list (hash 'ss_item_sk 1 'ss_ext_sales_price 100 'ss_sold_date_sk 1 'ss_addr_sk 1) (hash 'ss_item_sk 2 'ss_ext_sales_price 50 'ss_sold_date_sk 1 'ss_addr_sk 2)))
(define catalog_sales (list (hash 'cs_item_sk 1 'cs_ext_sales_price 20 'cs_sold_date_sk 1 'cs_bill_addr_sk 1)))
(define web_sales (list (hash 'ws_item_sk 1 'ws_ext_sales_price 30 'ws_sold_date_sk 1 'ws_bill_addr_sk 1)))
(define month 1)
(define year 2000)
(define union_sales (concat (for*/list ([ss store_sales] [d date_dim] [ca customer_address] [i item] #:when (and (equal? (hash-ref ss 'ss_sold_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref ss 'ss_addr_sk) (hash-ref ca 'ca_address_sk)) (equal? (hash-ref ss 'ss_item_sk) (hash-ref i 'i_item_sk)) (and (and (and (string=? (hash-ref i 'i_category) "Books") (equal? (hash-ref d 'd_year) year)) (equal? (hash-ref d 'd_moy) month)) (equal? (hash-ref ca 'ca_gmt_offset) (- 5))))) (hash 'manu (hash-ref i 'i_manufact_id) 'price (hash-ref ss 'ss_ext_sales_price))) (for*/list ([cs catalog_sales] [d date_dim] [ca customer_address] [i item] #:when (and (equal? (hash-ref cs 'cs_sold_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref cs 'cs_bill_addr_sk) (hash-ref ca 'ca_address_sk)) (equal? (hash-ref cs 'cs_item_sk) (hash-ref i 'i_item_sk)) (and (and (and (string=? (hash-ref i 'i_category) "Books") (equal? (hash-ref d 'd_year) year)) (equal? (hash-ref d 'd_moy) month)) (equal? (hash-ref ca 'ca_gmt_offset) (- 5))))) (hash 'manu (hash-ref i 'i_manufact_id) 'price (hash-ref cs 'cs_ext_sales_price))) (for*/list ([ws web_sales] [d date_dim] [ca customer_address] [i item] #:when (and (equal? (hash-ref ws 'ws_sold_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref ws 'ws_bill_addr_sk) (hash-ref ca 'ca_address_sk)) (equal? (hash-ref ws 'ws_item_sk) (hash-ref i 'i_item_sk)) (and (and (and (string=? (hash-ref i 'i_category) "Books") (equal? (hash-ref d 'd_year) year)) (equal? (hash-ref d 'd_moy) month)) (equal? (hash-ref ca 'ca_gmt_offset) (- 5))))) (hash 'manu (hash-ref i 'i_manufact_id) 'price (hash-ref ws 'ws_ext_sales_price)))))
(define result (let ([groups (make-hash)])
  (for* ([s union_sales]) (let* ([key (hash-ref s 'manu)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons s bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v))))) (string>? (let ([g a]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v)))) (let ([g b]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v)))))] [(string? (let ([g b]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v))))) (string>? (let ([g a]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v)))) (let ([g b]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v)))))] [else (> (let ([g a]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v)))) (let ([g b]) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v)))))]))))
  (for/list ([g _groups]) (hash 'i_manufact_id (hash-ref g 'key) 'total_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_manufact_id 1 'total_sales 150) (hash 'i_manufact_id 2 'total_sales 50))) (displayln "ok"))
