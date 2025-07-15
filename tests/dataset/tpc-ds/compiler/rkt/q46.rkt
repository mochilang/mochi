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

(define store_sales (list (hash 'ss_ticket_number 1 'ss_customer_sk 1 'ss_addr_sk 1 'ss_hdemo_sk 1 'ss_store_sk 1 'ss_sold_date_sk 1 'ss_coupon_amt 5 'ss_net_profit 20)))
(define date_dim (list (hash 'd_date_sk 1 'd_dow 6 'd_year 2020)))
(define store (list (hash 's_store_sk 1 's_city "CityA")))
(define household_demographics (list (hash 'hd_demo_sk 1 'hd_dep_count 2 'hd_vehicle_count 0)))
(define customer_address (list (hash 'ca_address_sk 1 'ca_city "Portland") (hash 'ca_address_sk 2 'ca_city "Seattle")))
(define customer (list (hash 'c_customer_sk 1 'c_last_name "Doe" 'c_first_name "John" 'c_current_addr_sk 2)))
(define depcnt 2)
(define vehcnt 0)
(define year 2020)
(define cities '("CityA"))
(define dn (let ([groups (make-hash)])
  (for* ([ss store_sales] [d date_dim] [s store] [hd household_demographics] [ca customer_address] #:when (and (equal? (hash-ref ss 'ss_sold_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref ss 'ss_store_sk) (hash-ref s 's_store_sk)) (equal? (hash-ref ss 'ss_hdemo_sk) (hash-ref hd 'hd_demo_sk)) (equal? (hash-ref ss 'ss_addr_sk) (hash-ref ca 'ca_address_sk)) (and (and (and (or (equal? (hash-ref hd 'hd_dep_count) depcnt) (equal? (hash-ref hd 'hd_vehicle_count) vehcnt)) (cond [(string? '(6 0)) (regexp-match? (regexp (hash-ref d 'd_dow)) '(6 0))] [(hash? '(6 0)) (hash-has-key? '(6 0) (hash-ref d 'd_dow))] [else (member (hash-ref d 'd_dow) '(6 0))])) (equal? (hash-ref d 'd_year) year)) (cond [(string? cities) (regexp-match? (regexp (hash-ref s 's_city)) cities)] [(hash? cities) (hash-has-key? cities (hash-ref s 's_city))] [else (member (hash-ref s 's_city) cities)])))) (let* ([key (hash 'ss_ticket_number (hash-ref ss 'ss_ticket_number) 'ss_customer_sk (hash-ref ss 'ss_customer_sk) 'ca_city (hash-ref ca 'ca_city))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ss ss 'd d 's s 'hd hd 'ca ca) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'ss_ticket_number (hash-ref (hash-ref g 'key) 'ss_ticket_number) 'ss_customer_sk (hash-ref (hash-ref g 'key) 'ss_customer_sk) 'bought_city (hash-ref (hash-ref g 'key) 'ca_city) 'amt (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_coupon_amt))]) v)) 'profit (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_net_profit))]) v))))))
(define base (let ([_items0 (for*/list ([dnrec dn] [c customer] [current_addr customer_address] #:when (and (equal? (hash-ref dnrec 'ss_customer_sk) (hash-ref c 'c_customer_sk)) (equal? (hash-ref c 'c_current_addr_sk) (hash-ref current_addr 'ca_address_sk)) (not (equal? (hash-ref current_addr 'ca_city) (hash-ref dnrec 'bought_city))))) (hash 'dnrec dnrec 'c c 'current_addr current_addr))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((dnrec (hash-ref a 'dnrec)) (c (hash-ref a 'c)) (current_addr (hash-ref a 'current_addr))) (list (hash-ref c 'c_last_name) (hash-ref c 'c_first_name) (hash-ref current_addr 'ca_city) (hash-ref dnrec 'bought_city) (hash-ref dnrec 'ss_ticket_number)))) (string<? (let ((dnrec (hash-ref a 'dnrec)) (c (hash-ref a 'c)) (current_addr (hash-ref a 'current_addr))) (list (hash-ref c 'c_last_name) (hash-ref c 'c_first_name) (hash-ref current_addr 'ca_city) (hash-ref dnrec 'bought_city) (hash-ref dnrec 'ss_ticket_number))) (let ((dnrec (hash-ref b 'dnrec)) (c (hash-ref b 'c)) (current_addr (hash-ref b 'current_addr))) (list (hash-ref c 'c_last_name) (hash-ref c 'c_first_name) (hash-ref current_addr 'ca_city) (hash-ref dnrec 'bought_city) (hash-ref dnrec 'ss_ticket_number))))] [(string? (let ((dnrec (hash-ref b 'dnrec)) (c (hash-ref b 'c)) (current_addr (hash-ref b 'current_addr))) (list (hash-ref c 'c_last_name) (hash-ref c 'c_first_name) (hash-ref current_addr 'ca_city) (hash-ref dnrec 'bought_city) (hash-ref dnrec 'ss_ticket_number)))) (string<? (let ((dnrec (hash-ref a 'dnrec)) (c (hash-ref a 'c)) (current_addr (hash-ref a 'current_addr))) (list (hash-ref c 'c_last_name) (hash-ref c 'c_first_name) (hash-ref current_addr 'ca_city) (hash-ref dnrec 'bought_city) (hash-ref dnrec 'ss_ticket_number))) (let ((dnrec (hash-ref b 'dnrec)) (c (hash-ref b 'c)) (current_addr (hash-ref b 'current_addr))) (list (hash-ref c 'c_last_name) (hash-ref c 'c_first_name) (hash-ref current_addr 'ca_city) (hash-ref dnrec 'bought_city) (hash-ref dnrec 'ss_ticket_number))))] [else (< (let ((dnrec (hash-ref a 'dnrec)) (c (hash-ref a 'c)) (current_addr (hash-ref a 'current_addr))) (list (hash-ref c 'c_last_name) (hash-ref c 'c_first_name) (hash-ref current_addr 'ca_city) (hash-ref dnrec 'bought_city) (hash-ref dnrec 'ss_ticket_number))) (let ((dnrec (hash-ref b 'dnrec)) (c (hash-ref b 'c)) (current_addr (hash-ref b 'current_addr))) (list (hash-ref c 'c_last_name) (hash-ref c 'c_first_name) (hash-ref current_addr 'ca_city) (hash-ref dnrec 'bought_city) (hash-ref dnrec 'ss_ticket_number))))]))))
  (for/list ([item _items0]) (let ((dnrec (hash-ref item 'dnrec)) (c (hash-ref item 'c)) (current_addr (hash-ref item 'current_addr))) (hash 'c_last_name (hash-ref c 'c_last_name) 'c_first_name (hash-ref c 'c_first_name) 'ca_city (hash-ref current_addr 'ca_city) 'bought_city (hash-ref dnrec 'bought_city) 'ss_ticket_number (hash-ref dnrec 'ss_ticket_number) 'amt (hash-ref dnrec 'amt) 'profit (hash-ref dnrec 'profit))))))
(define result base)
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'c_last_name "Doe" 'c_first_name "John" 'ca_city "Seattle" 'bought_city "Portland" 'ss_ticket_number 1 'amt 5 'profit 20))) (displayln "ok"))
