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

(define customer (list (hash 'c_customer_sk 1 'c_customer_id 1 'c_first_name "Alice" 'c_last_name "Smith")))
(define date_dim (list (hash 'd_date_sk 1 'd_year 1998) (hash 'd_date_sk 2 'd_year 1999)))
(define store_sales (list (hash 'ss_customer_sk 1 'ss_sold_date_sk 1 'ss_net_paid 100) (hash 'ss_customer_sk 1 'ss_sold_date_sk 2 'ss_net_paid 110)))
(define web_sales (list (hash 'ws_bill_customer_sk 1 'ws_sold_date_sk 1 'ws_net_paid 40) (hash 'ws_bill_customer_sk 1 'ws_sold_date_sk 2 'ws_net_paid 80)))
(define year_total (concat (let ([groups (make-hash)])
  (for* ([c customer] [ss store_sales] [d date_dim] #:when (and (equal? (hash-ref c 'c_customer_sk) (hash-ref ss 'ss_customer_sk)) (equal? (hash-ref d 'd_date_sk) (hash-ref ss 'ss_sold_date_sk)) (or (equal? (hash-ref d 'd_year) 1998) (equal? (hash-ref d 'd_year) 1999)))) (let* ([key (hash 'id (hash-ref c 'c_customer_id) 'first (hash-ref c 'c_first_name) 'last (hash-ref c 'c_last_name) 'year (hash-ref d 'd_year))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'c c 'ss ss 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'customer_id (hash-ref (hash-ref g 'key) 'id) 'customer_first_name (hash-ref (hash-ref g 'key) 'first) 'customer_last_name (hash-ref (hash-ref g 'key) 'last) 'year (hash-ref (hash-ref g 'key) 'year) 'year_total (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_net_paid))]) v)) 'sale_type "s"))) (let ([groups (make-hash)])
  (for* ([c customer] [ws web_sales] [d date_dim] #:when (and (equal? (hash-ref c 'c_customer_sk) (hash-ref ws 'ws_bill_customer_sk)) (equal? (hash-ref d 'd_date_sk) (hash-ref ws 'ws_sold_date_sk)) (or (equal? (hash-ref d 'd_year) 1998) (equal? (hash-ref d 'd_year) 1999)))) (let* ([key (hash 'id (hash-ref c 'c_customer_id) 'first (hash-ref c 'c_first_name) 'last (hash-ref c 'c_last_name) 'year (hash-ref d 'd_year))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'c c 'ws ws 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'customer_id (hash-ref (hash-ref g 'key) 'id) 'customer_first_name (hash-ref (hash-ref g 'key) 'first) 'customer_last_name (hash-ref (hash-ref g 'key) 'last) 'year (hash-ref (hash-ref g 'key) 'year) 'year_total (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ws) 'ws_net_paid))]) v)) 'sale_type "w")))))
(define s_firstyear (first (for*/list ([y year_total] #:when (and (and (string=? (hash-ref y 'sale_type) "s") (equal? (hash-ref y 'year) 1998)))) y)))
(define s_secyear (first (for*/list ([y year_total] #:when (and (and (string=? (hash-ref y 'sale_type) "s") (equal? (hash-ref y 'year) 1999)))) y)))
(define w_firstyear (first (for*/list ([y year_total] #:when (and (and (string=? (hash-ref y 'sale_type) "w") (equal? (hash-ref y 'year) 1998)))) y)))
(define w_secyear (first (for*/list ([y year_total] #:when (and (and (string=? (hash-ref y 'sale_type) "w") (equal? (hash-ref y 'year) 1999)))) y)))
(define result (if (and (and (_gt (hash-ref s_firstyear 'year_total) 0) (_gt (hash-ref w_firstyear 'year_total) 0)) (_gt (/ (hash-ref w_secyear 'year_total) (hash-ref w_firstyear 'year_total)) (/ (hash-ref s_secyear 'year_total) (hash-ref s_firstyear 'year_total)))) (list (hash 'customer_id (hash-ref s_secyear 'customer_id) 'customer_first_name (hash-ref s_secyear 'customer_first_name) 'customer_last_name (hash-ref s_secyear 'customer_last_name))) '()))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'customer_id 1 'customer_first_name "Alice" 'customer_last_name "Smith"))) (displayln "ok"))
