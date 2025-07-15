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

(define store_sales (list (hash 'ss_ticket_number 1 'ss_customer_sk 1 'ss_sold_date_sk 1 'ss_store_sk 1 'ss_hdemo_sk 1)))
(define date_dim (list (hash 'd_date_sk 1 'd_dom 1 'd_year 1998)))
(define store (list (hash 's_store_sk 1 's_county "A")))
(define household_demographics (list (hash 'hd_demo_sk 1 'hd_buy_potential "1001-5000" 'hd_vehicle_count 2 'hd_dep_count 3)))
(define customer (list (hash 'c_customer_sk 1 'c_last_name "Smith" 'c_first_name "Alice" 'c_salutation "Ms." 'c_preferred_cust_flag "Y")))
(define groups (let ([groups (make-hash)])
  (for* ([ss store_sales] [d date_dim] [s store] [hd household_demographics] #:when (and (equal? (hash-ref d 'd_date_sk) (hash-ref ss 'ss_sold_date_sk)) (equal? (hash-ref s 's_store_sk) (hash-ref ss 'ss_store_sk)) (equal? (hash-ref hd 'hd_demo_sk) (hash-ref ss 'ss_hdemo_sk)) (and (and (and (and (and (and (_ge (hash-ref d 'd_dom) 1) (_le (hash-ref d 'd_dom) 2)) (or (string=? (hash-ref hd 'hd_buy_potential) "1001-5000") (string=? (hash-ref hd 'hd_buy_potential) "0-500"))) (_gt (hash-ref hd 'hd_vehicle_count) 0)) (_gt (/ (hash-ref hd 'hd_dep_count) (hash-ref hd 'hd_vehicle_count)) 1)) (or (or (equal? (hash-ref d 'd_year) 1998) (equal? (hash-ref d 'd_year) 1999)) (equal? (hash-ref d 'd_year) 2000))) (string=? (hash-ref s 's_county) "A")))) (let* ([key (hash 'ticket (hash-ref ss 'ss_ticket_number) 'cust (hash-ref ss 'ss_customer_sk))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ss ss 'd d 's s 'hd hd) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'key (hash-ref g 'key) 'cnt (length (hash-ref g 'items))))))
(define result (let ([_items0 (for*/list ([g groups] [c customer] #:when (and (equal? (hash-ref c 'c_customer_sk) (hash-ref (hash-ref g 'key) 'cust)) (and (_ge (hash-ref g 'cnt) 1) (_le (hash-ref g 'cnt) 5)))) (hash 'g g 'c c))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((g (hash-ref a 'g)) (c (hash-ref a 'c))) (list (- (hash-ref g 'cnt)) (hash-ref c 'c_last_name)))) (string<? (let ((g (hash-ref a 'g)) (c (hash-ref a 'c))) (list (- (hash-ref g 'cnt)) (hash-ref c 'c_last_name))) (let ((g (hash-ref b 'g)) (c (hash-ref b 'c))) (list (- (hash-ref g 'cnt)) (hash-ref c 'c_last_name))))] [(string? (let ((g (hash-ref b 'g)) (c (hash-ref b 'c))) (list (- (hash-ref g 'cnt)) (hash-ref c 'c_last_name)))) (string<? (let ((g (hash-ref a 'g)) (c (hash-ref a 'c))) (list (- (hash-ref g 'cnt)) (hash-ref c 'c_last_name))) (let ((g (hash-ref b 'g)) (c (hash-ref b 'c))) (list (- (hash-ref g 'cnt)) (hash-ref c 'c_last_name))))] [else (< (let ((g (hash-ref a 'g)) (c (hash-ref a 'c))) (list (- (hash-ref g 'cnt)) (hash-ref c 'c_last_name))) (let ((g (hash-ref b 'g)) (c (hash-ref b 'c))) (list (- (hash-ref g 'cnt)) (hash-ref c 'c_last_name))))]))))
  (for/list ([item _items0]) (let ((g (hash-ref item 'g)) (c (hash-ref item 'c))) (hash 'c_last_name (hash-ref c 'c_last_name) 'c_first_name (hash-ref c 'c_first_name) 'c_salutation (hash-ref c 'c_salutation) 'c_preferred_cust_flag (hash-ref c 'c_preferred_cust_flag) 'ss_ticket_number (hash-ref (hash-ref g 'key) 'ticket) 'cnt (hash-ref g 'cnt))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'c_last_name "Smith" 'c_first_name "Alice" 'c_salutation "Ms." 'c_preferred_cust_flag "Y" 'ss_ticket_number 1 'cnt 1))) (displayln "ok"))
