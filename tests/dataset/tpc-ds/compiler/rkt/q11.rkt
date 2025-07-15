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

(struct Customer (c_customer_sk c_customer_id c_first_name c_last_name) #:transparent #:mutable)
(struct StoreSale (ss_customer_sk ss_sold_date_sk ss_ext_list_price) #:transparent #:mutable)
(struct WebSale (ws_bill_customer_sk ws_sold_date_sk ws_ext_list_price) #:transparent #:mutable)
(define customer (list (hash 'c_customer_sk 1 'c_customer_id "C1" 'c_first_name "John" 'c_last_name "Doe")))
(define store_sales (list (hash 'ss_customer_sk 1 'ss_sold_date_sk 1998 'ss_ext_list_price 60) (hash 'ss_customer_sk 1 'ss_sold_date_sk 1999 'ss_ext_list_price 90)))
(define web_sales (list (hash 'ws_bill_customer_sk 1 'ws_sold_date_sk 1998 'ws_ext_list_price 50) (hash 'ws_bill_customer_sk 1 'ws_sold_date_sk 1999 'ws_ext_list_price 150)))
(define ss98 (apply + (for*/list ([v (for*/list ([ss store_sales] #:when (and (equal? (hash-ref ss 'ss_sold_date_sk) 1998))) (hash-ref ss 'ss_ext_list_price))]) v)))
(define ss99 (apply + (for*/list ([v (for*/list ([ss store_sales] #:when (and (equal? (hash-ref ss 'ss_sold_date_sk) 1999))) (hash-ref ss 'ss_ext_list_price))]) v)))
(define ws98 (apply + (for*/list ([v (for*/list ([ws web_sales] #:when (and (equal? (hash-ref ws 'ws_sold_date_sk) 1998))) (hash-ref ws 'ws_ext_list_price))]) v)))
(define ws99 (apply + (for*/list ([v (for*/list ([ws web_sales] #:when (and (equal? (hash-ref ws 'ws_sold_date_sk) 1999))) (hash-ref ws 'ws_ext_list_price))]) v)))
(define growth_ok (and (and (_gt ws98 0) (_gt ss98 0)) (_gt (/ ws99 ws98) (/ ss99 ss98))))
(define result (if growth_ok (list (hash 'customer_id "C1" 'customer_first_name "John" 'customer_last_name "Doe")) '()))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'customer_id "C1" 'customer_first_name "John" 'customer_last_name "Doe"))) (displayln "ok"))
