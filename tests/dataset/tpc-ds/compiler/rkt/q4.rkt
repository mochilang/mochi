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

(define customer (list (hash 'c_customer_sk 1 'c_customer_id "C1" 'c_first_name "Alice" 'c_last_name "A" 'c_login "alice")))
(define store_sales (list (hash 'ss_customer_sk 1 'ss_sold_date_sk 1 'ss_ext_list_price 10 'ss_ext_wholesale_cost 5 'ss_ext_discount_amt 0 'ss_ext_sales_price 10) (hash 'ss_customer_sk 1 'ss_sold_date_sk 2 'ss_ext_list_price 20 'ss_ext_wholesale_cost 5 'ss_ext_discount_amt 0 'ss_ext_sales_price 20)))
(define catalog_sales (list (hash 'cs_bill_customer_sk 1 'cs_sold_date_sk 1 'cs_ext_list_price 10 'cs_ext_wholesale_cost 2 'cs_ext_discount_amt 0 'cs_ext_sales_price 10) (hash 'cs_bill_customer_sk 1 'cs_sold_date_sk 2 'cs_ext_list_price 30 'cs_ext_wholesale_cost 2 'cs_ext_discount_amt 0 'cs_ext_sales_price 30)))
(define web_sales (list (hash 'ws_bill_customer_sk 1 'ws_sold_date_sk 1 'ws_ext_list_price 10 'ws_ext_wholesale_cost 5 'ws_ext_discount_amt 0 'ws_ext_sales_price 10) (hash 'ws_bill_customer_sk 1 'ws_sold_date_sk 2 'ws_ext_list_price 12 'ws_ext_wholesale_cost 5 'ws_ext_discount_amt 0 'ws_ext_sales_price 12)))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2001) (hash 'd_date_sk 2 'd_year 2002)))
(define year_total (append (append (let ([groups (make-hash)])
  (for* ([c customer] [s store_sales] [d date_dim] #:when (and (equal? (hash-ref c 'c_customer_sk) (hash-ref s 'ss_customer_sk)) (equal? (hash-ref s 'ss_sold_date_sk) (hash-ref d 'd_date_sk)))) (let* ([key (hash 'id (hash-ref c 'c_customer_id) 'first (hash-ref c 'c_first_name) 'last (hash-ref c 'c_last_name) 'login (hash-ref c 'c_login) 'year (hash-ref d 'd_year))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'c c 's s 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'customer_id (hash-ref (hash-ref g 'key) 'id) 'customer_first_name (hash-ref (hash-ref g 'key) 'first) 'customer_last_name (hash-ref (hash-ref g 'key) 'last) 'customer_login (hash-ref (hash-ref g 'key) 'login) 'dyear (hash-ref (hash-ref g 'key) 'year) 'year_total (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (/ (+ (- (- (hash-ref x 'ss_ext_list_price) (hash-ref x 'ss_ext_wholesale_cost)) (hash-ref x 'ss_ext_discount_amt)) (hash-ref x 'ss_ext_sales_price)) 2))]) v)) 'sale_type "s"))) (let ([groups (make-hash)])
  (for* ([c customer] [cs catalog_sales] [d date_dim] #:when (and (equal? (hash-ref c 'c_customer_sk) (hash-ref cs 'cs_bill_customer_sk)) (equal? (hash-ref cs 'cs_sold_date_sk) (hash-ref d 'd_date_sk)))) (let* ([key (hash 'id (hash-ref c 'c_customer_id) 'first (hash-ref c 'c_first_name) 'last (hash-ref c 'c_last_name) 'login (hash-ref c 'c_login) 'year (hash-ref d 'd_year))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'c c 'cs cs 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'customer_id (hash-ref (hash-ref g 'key) 'id) 'customer_first_name (hash-ref (hash-ref g 'key) 'first) 'customer_last_name (hash-ref (hash-ref g 'key) 'last) 'customer_login (hash-ref (hash-ref g 'key) 'login) 'dyear (hash-ref (hash-ref g 'key) 'year) 'year_total (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (/ (+ (- (- (hash-ref x 'cs_ext_list_price) (hash-ref x 'cs_ext_wholesale_cost)) (hash-ref x 'cs_ext_discount_amt)) (hash-ref x 'cs_ext_sales_price)) 2))]) v)) 'sale_type "c")))) (let ([groups (make-hash)])
  (for* ([c customer] [ws web_sales] [d date_dim] #:when (and (equal? (hash-ref c 'c_customer_sk) (hash-ref ws 'ws_bill_customer_sk)) (equal? (hash-ref ws 'ws_sold_date_sk) (hash-ref d 'd_date_sk)))) (let* ([key (hash 'id (hash-ref c 'c_customer_id) 'first (hash-ref c 'c_first_name) 'last (hash-ref c 'c_last_name) 'login (hash-ref c 'c_login) 'year (hash-ref d 'd_year))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'c c 'ws ws 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'customer_id (hash-ref (hash-ref g 'key) 'id) 'customer_first_name (hash-ref (hash-ref g 'key) 'first) 'customer_last_name (hash-ref (hash-ref g 'key) 'last) 'customer_login (hash-ref (hash-ref g 'key) 'login) 'dyear (hash-ref (hash-ref g 'key) 'year) 'year_total (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (/ (+ (- (- (hash-ref x 'ws_ext_list_price) (hash-ref x 'ws_ext_wholesale_cost)) (hash-ref x 'ws_ext_discount_amt)) (hash-ref x 'ws_ext_sales_price)) 2))]) v)) 'sale_type "w")))))
(define result (let ([_items0 (for*/list ([s1 year_total] [s2 year_total] [c1 year_total] [c2 year_total] [w1 year_total] [w2 year_total] #:when (and (equal? (hash-ref s2 'customer_id) (hash-ref s1 'customer_id)) (equal? (hash-ref c1 'customer_id) (hash-ref s1 'customer_id)) (equal? (hash-ref c2 'customer_id) (hash-ref s1 'customer_id)) (equal? (hash-ref w1 'customer_id) (hash-ref s1 'customer_id)) (equal? (hash-ref w2 'customer_id) (hash-ref s1 'customer_id)) (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (string=? (hash-ref s1 'sale_type) "s") (string=? (hash-ref c1 'sale_type) "c")) (string=? (hash-ref w1 'sale_type) "w")) (string=? (hash-ref s2 'sale_type) "s")) (string=? (hash-ref c2 'sale_type) "c")) (string=? (hash-ref w2 'sale_type) "w")) (equal? (hash-ref s1 'dyear) 2001)) (equal? (hash-ref s2 'dyear) 2002)) (equal? (hash-ref c1 'dyear) 2001)) (equal? (hash-ref c2 'dyear) 2002)) (equal? (hash-ref w1 'dyear) 2001)) (equal? (hash-ref w2 'dyear) 2002)) (_gt (hash-ref s1 'year_total) 0)) (_gt (hash-ref c1 'year_total) 0)) (_gt (hash-ref w1 'year_total) 0)) (_gt (if (_gt (hash-ref c1 'year_total) 0) (/ (hash-ref c2 'year_total) (hash-ref c1 'year_total)) '()) (if (_gt (hash-ref s1 'year_total) 0) (/ (hash-ref s2 'year_total) (hash-ref s1 'year_total)) '()))) (_gt (if (_gt (hash-ref c1 'year_total) 0) (/ (hash-ref c2 'year_total) (hash-ref c1 'year_total)) '()) (if (_gt (hash-ref w1 'year_total) 0) (/ (hash-ref w2 'year_total) (hash-ref w1 'year_total)) '()))))) (hash 's1 s1 's2 s2 'c1 c1 'c2 c2 'w1 w1 'w2 w2))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((s1 (hash-ref a 's1)) (s2 (hash-ref a 's2)) (c1 (hash-ref a 'c1)) (c2 (hash-ref a 'c2)) (w1 (hash-ref a 'w1)) (w2 (hash-ref a 'w2))) (list (hash-ref s2 'customer_id) (hash-ref s2 'customer_first_name) (hash-ref s2 'customer_last_name) (hash-ref s2 'customer_login)))) (string<? (let ((s1 (hash-ref a 's1)) (s2 (hash-ref a 's2)) (c1 (hash-ref a 'c1)) (c2 (hash-ref a 'c2)) (w1 (hash-ref a 'w1)) (w2 (hash-ref a 'w2))) (list (hash-ref s2 'customer_id) (hash-ref s2 'customer_first_name) (hash-ref s2 'customer_last_name) (hash-ref s2 'customer_login))) (let ((s1 (hash-ref b 's1)) (s2 (hash-ref b 's2)) (c1 (hash-ref b 'c1)) (c2 (hash-ref b 'c2)) (w1 (hash-ref b 'w1)) (w2 (hash-ref b 'w2))) (list (hash-ref s2 'customer_id) (hash-ref s2 'customer_first_name) (hash-ref s2 'customer_last_name) (hash-ref s2 'customer_login))))] [(string? (let ((s1 (hash-ref b 's1)) (s2 (hash-ref b 's2)) (c1 (hash-ref b 'c1)) (c2 (hash-ref b 'c2)) (w1 (hash-ref b 'w1)) (w2 (hash-ref b 'w2))) (list (hash-ref s2 'customer_id) (hash-ref s2 'customer_first_name) (hash-ref s2 'customer_last_name) (hash-ref s2 'customer_login)))) (string<? (let ((s1 (hash-ref a 's1)) (s2 (hash-ref a 's2)) (c1 (hash-ref a 'c1)) (c2 (hash-ref a 'c2)) (w1 (hash-ref a 'w1)) (w2 (hash-ref a 'w2))) (list (hash-ref s2 'customer_id) (hash-ref s2 'customer_first_name) (hash-ref s2 'customer_last_name) (hash-ref s2 'customer_login))) (let ((s1 (hash-ref b 's1)) (s2 (hash-ref b 's2)) (c1 (hash-ref b 'c1)) (c2 (hash-ref b 'c2)) (w1 (hash-ref b 'w1)) (w2 (hash-ref b 'w2))) (list (hash-ref s2 'customer_id) (hash-ref s2 'customer_first_name) (hash-ref s2 'customer_last_name) (hash-ref s2 'customer_login))))] [else (< (let ((s1 (hash-ref a 's1)) (s2 (hash-ref a 's2)) (c1 (hash-ref a 'c1)) (c2 (hash-ref a 'c2)) (w1 (hash-ref a 'w1)) (w2 (hash-ref a 'w2))) (list (hash-ref s2 'customer_id) (hash-ref s2 'customer_first_name) (hash-ref s2 'customer_last_name) (hash-ref s2 'customer_login))) (let ((s1 (hash-ref b 's1)) (s2 (hash-ref b 's2)) (c1 (hash-ref b 'c1)) (c2 (hash-ref b 'c2)) (w1 (hash-ref b 'w1)) (w2 (hash-ref b 'w2))) (list (hash-ref s2 'customer_id) (hash-ref s2 'customer_first_name) (hash-ref s2 'customer_last_name) (hash-ref s2 'customer_login))))]))))
  (for/list ([item _items0]) (let ((s1 (hash-ref item 's1)) (s2 (hash-ref item 's2)) (c1 (hash-ref item 'c1)) (c2 (hash-ref item 'c2)) (w1 (hash-ref item 'w1)) (w2 (hash-ref item 'w2))) (hash 'customer_id (hash-ref s2 'customer_id) 'customer_first_name (hash-ref s2 'customer_first_name) 'customer_last_name (hash-ref s2 'customer_last_name) 'customer_login (hash-ref s2 'customer_login))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'customer_id "C1" 'customer_first_name "Alice" 'customer_last_name "A" 'customer_login "alice"))) (displayln "ok"))
