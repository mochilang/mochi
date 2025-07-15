#lang racket
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

(define customer_address (list (hash 'ca_address_sk 1 'ca_state "CA" 'ca_zip "12345")))
(define customer (list (hash 'c_customer_sk 1 'c_current_addr_sk 1)))
(define store_sales (list (hash 'ss_customer_sk 1 'ss_sold_date_sk 1 'ss_item_sk 1) (hash 'ss_customer_sk 1 'ss_sold_date_sk 1 'ss_item_sk 1) (hash 'ss_customer_sk 1 'ss_sold_date_sk 1 'ss_item_sk 1) (hash 'ss_customer_sk 1 'ss_sold_date_sk 1 'ss_item_sk 1) (hash 'ss_customer_sk 1 'ss_sold_date_sk 1 'ss_item_sk 1) (hash 'ss_customer_sk 1 'ss_sold_date_sk 1 'ss_item_sk 1) (hash 'ss_customer_sk 1 'ss_sold_date_sk 1 'ss_item_sk 1) (hash 'ss_customer_sk 1 'ss_sold_date_sk 1 'ss_item_sk 1) (hash 'ss_customer_sk 1 'ss_sold_date_sk 1 'ss_item_sk 1) (hash 'ss_customer_sk 1 'ss_sold_date_sk 1 'ss_item_sk 1)))
(define date_dim (list (hash 'd_date_sk 1 'd_year 1999 'd_moy 5 'd_month_seq 120)))
(define item (list (hash 'i_item_sk 1 'i_category "A" 'i_current_price 100) (hash 'i_item_sk 2 'i_category "A" 'i_current_price 50)))
(define target_month_seq (_max (for*/list ([d date_dim] #:when (and (and (equal? (hash-ref d 'd_year) 1999) (equal? (hash-ref d 'd_moy) 5)))) (hash-ref d 'd_month_seq))))
(define result (let ([groups (make-hash)])
  (for* ([a customer_address] [c customer] [s store_sales] [d date_dim] [i item] #:when (and (equal? (hash-ref a 'ca_address_sk) (hash-ref c 'c_current_addr_sk)) (equal? (hash-ref c 'c_customer_sk) (hash-ref s 'ss_customer_sk)) (equal? (hash-ref s 'ss_sold_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref s 'ss_item_sk) (hash-ref i 'i_item_sk)) (and (equal? (hash-ref d 'd_month_seq) target_month_seq) (_gt (hash-ref i 'i_current_price) (* 1.2 (let ([xs (for*/list ([j item] #:when (and (equal? (hash-ref j 'i_category) (hash-ref i 'i_category)))) (hash-ref j 'i_current_price))] [n (length (for*/list ([j item] #:when (and (equal? (hash-ref j 'i_category) (hash-ref i 'i_category)))) (hash-ref j 'i_current_price)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))))) (let* ([key (hash-ref a 'ca_state)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'a a 'c c 's s 'd d 'i i) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (list (length (hash-ref g 'items)) (hash-ref g 'key)))) (string>? (let ([g a]) (list (length (hash-ref g 'items)) (hash-ref g 'key))) (let ([g b]) (list (length (hash-ref g 'items)) (hash-ref g 'key))))] [(string? (let ([g b]) (list (length (hash-ref g 'items)) (hash-ref g 'key)))) (string>? (let ([g a]) (list (length (hash-ref g 'items)) (hash-ref g 'key))) (let ([g b]) (list (length (hash-ref g 'items)) (hash-ref g 'key))))] [else (> (let ([g a]) (list (length (hash-ref g 'items)) (hash-ref g 'key))) (let ([g b]) (list (length (hash-ref g 'items)) (hash-ref g 'key))))]))))
  (set! _groups (filter (lambda (g) (_ge (length (hash-ref g 'items)) 10)) _groups))
  (for/list ([g _groups]) (hash 'state (hash-ref g 'key) 'cnt (length (hash-ref g 'items))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'state "CA" 'cnt 10))) (displayln "ok"))
