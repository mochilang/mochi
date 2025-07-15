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

(struct StoreSale (ss_sold_date_sk ss_item_sk ss_customer_sk ss_store_sk ss_ext_sales_price) #:transparent #:mutable)
(struct DateDim (d_date_sk d_year d_moy) #:transparent #:mutable)
(struct Item (i_item_sk i_brand_id i_brand i_manufact_id i_manufact i_manager_id) #:transparent #:mutable)
(struct Customer (c_customer_sk c_current_addr_sk) #:transparent #:mutable)
(struct CustomerAddress (ca_address_sk ca_zip) #:transparent #:mutable)
(struct Store (s_store_sk s_zip) #:transparent #:mutable)
(define store_sales (list (hash 'ss_sold_date_sk 1 'ss_item_sk 1 'ss_customer_sk 1 'ss_store_sk 1 'ss_ext_sales_price 100)))
(define date_dim (list (hash 'd_date_sk 1 'd_year 1999 'd_moy 11)))
(define item (list (hash 'i_item_sk 1 'i_brand_id 1 'i_brand "B1" 'i_manufact_id 1 'i_manufact "M1" 'i_manager_id 10)))
(define customer (list (hash 'c_customer_sk 1 'c_current_addr_sk 1)))
(define customer_address (list (hash 'ca_address_sk 1 'ca_zip "11111")))
(define store (list (hash 's_store_sk 1 's_zip "99999")))
(define result (let ([groups (make-hash)])
  (for* ([d date_dim] [ss store_sales] [i item] [c customer] [ca customer_address] [s store] #:when (and (equal? (hash-ref ss 'ss_sold_date_sk) (hash-ref d 'd_date_sk)) (and (equal? (hash-ref ss 'ss_item_sk) (hash-ref i 'i_item_sk)) (equal? (hash-ref i 'i_manager_id) 10)) (equal? (hash-ref ss 'ss_customer_sk) (hash-ref c 'c_customer_sk)) (equal? (hash-ref c 'c_current_addr_sk) (hash-ref ca 'ca_address_sk)) (and (equal? (hash-ref ss 'ss_store_sk) (hash-ref s 's_store_sk)) (not (equal? (substr (hash-ref ca 'ca_zip) 0 5) (substr (hash-ref s 's_zip) 0 5)))) (and (equal? (hash-ref d 'd_moy) 11) (equal? (hash-ref d 'd_year) 1999)))) (let* ([key (hash 'brand (hash-ref i 'i_brand) 'brand_id (hash-ref i 'i_brand_id) 'man_id (hash-ref i 'i_manufact_id) 'man (hash-ref i 'i_manufact))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'd d 'ss ss 'i i 'c c 'ca ca 's s) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'brand)))) (string>? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'brand))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'brand))))] [(string? (let ([g b]) (list (hash-ref (hash-ref g 'key) 'brand)))) (string>? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'brand))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'brand))))] [else (> (let ([g a]) (list (hash-ref (hash-ref g 'key) 'brand))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'brand))))]))))
  (for/list ([g _groups]) (hash 'i_brand (hash-ref (hash-ref g 'key) 'brand) 'i_brand_id (hash-ref (hash-ref g 'key) 'brand_id) 'i_manufact_id (hash-ref (hash-ref g 'key) 'man_id) 'i_manufact (hash-ref (hash-ref g 'key) 'man) 'ext_price (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_sales_price))]) v))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_brand "B1" 'i_brand_id 1 'i_manufact_id 1 'i_manufact "M1" 'ext_price 100))) (displayln "ok"))
