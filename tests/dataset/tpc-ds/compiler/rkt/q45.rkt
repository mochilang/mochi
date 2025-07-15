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

(define web_sales (list (hash 'bill_customer_sk 1 'item_sk 1 'sold_date_sk 1 'sales_price 50) (hash 'bill_customer_sk 2 'item_sk 2 'sold_date_sk 1 'sales_price 30)))
(define customer (list (hash 'c_customer_sk 1 'c_current_addr_sk 1) (hash 'c_customer_sk 2 'c_current_addr_sk 2)))
(define customer_address (list (hash 'ca_address_sk 1 'ca_zip "85669") (hash 'ca_address_sk 2 'ca_zip "99999")))
(define item (list (hash 'i_item_sk 1 'i_item_id "I1") (hash 'i_item_sk 2 'i_item_id "I2")))
(define date_dim (list (hash 'd_date_sk 1 'd_qoy 1 'd_year 2020)))
(define zip_list '("85669" "86197" "88274" "83405" "86475" "85392" "85460" "80348" "81792"))
(define item_ids '("I2"))
(define qoy 1)
(define year 2020)
(define base (let ([groups (make-hash)])
  (for* ([ws web_sales] [c customer] [ca customer_address] [i item] [d date_dim] #:when (and (equal? (hash-ref ws 'bill_customer_sk) (hash-ref c 'c_customer_sk)) (equal? (hash-ref c 'c_current_addr_sk) (hash-ref ca 'ca_address_sk)) (equal? (hash-ref ws 'item_sk) (hash-ref i 'i_item_sk)) (equal? (hash-ref ws 'sold_date_sk) (hash-ref d 'd_date_sk)) (and (and (or (cond [(string? zip_list) (regexp-match? (regexp (substr (hash-ref ca 'ca_zip) 0 5)) zip_list)] [(hash? zip_list) (hash-has-key? zip_list (substr (hash-ref ca 'ca_zip) 0 5))] [else (member (substr (hash-ref ca 'ca_zip) 0 5) zip_list)]) (cond [(string? item_ids) (regexp-match? (regexp (hash-ref i 'i_item_id)) item_ids)] [(hash? item_ids) (hash-has-key? item_ids (hash-ref i 'i_item_id))] [else (member (hash-ref i 'i_item_id) item_ids)])) (equal? (hash-ref d 'd_qoy) qoy)) (equal? (hash-ref d 'd_year) year)))) (let* ([key (hash-ref ca 'ca_zip)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ws ws 'c c 'ca ca 'i i 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'ca_zip (hash-ref g 'key) 'sum_ws_sales_price (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ws) 'sales_price))]) v))))))
(define records base)
(displayln (jsexpr->string (_json-fix records)))
(when (equal? records (list (hash 'ca_zip "85669" 'sum_ws_sales_price 50) (hash 'ca_zip "99999" 'sum_ws_sales_price 30))) (displayln "ok"))
