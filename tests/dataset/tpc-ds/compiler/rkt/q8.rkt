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

(define store_sales (list (hash 'ss_store_sk 1 'ss_sold_date_sk 1 'ss_net_profit 10)))
(define date_dim (list (hash 'd_date_sk 1 'd_qoy 1 'd_year 1998)))
(define store (list (hash 's_store_sk 1 's_store_name "Store1" 's_zip "12345")))
(define customer_address (list (hash 'ca_address_sk 1 'ca_zip "12345")))
(define customer (list (hash 'c_customer_sk 1 'c_current_addr_sk 1 'c_preferred_cust_flag "Y")))
(reverse (substr "zip" 0 2))
(define zip_list '("12345"))
(define result (let ([groups (make-hash)])
  (for* ([ss store_sales] [d date_dim] [s store] [ca customer_address] [c customer] #:when (and (and (and (equal? (hash-ref ss 'ss_sold_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref d 'd_qoy) 1)) (equal? (hash-ref d 'd_year) 1998)) (equal? (hash-ref ss 'ss_store_sk) (hash-ref s 's_store_sk)) (equal? (substr (hash-ref s 's_zip) 0 2) (substr (hash-ref ca 'ca_zip) 0 2)) (and (equal? (hash-ref ca 'ca_address_sk) (hash-ref c 'c_current_addr_sk)) (string=? (hash-ref c 'c_preferred_cust_flag) "Y")) (cond [(string? zip_list) (regexp-match? (regexp (substr (hash-ref ca 'ca_zip) 0 5)) zip_list)] [(hash? zip_list) (hash-has-key? zip_list (substr (hash-ref ca 'ca_zip) 0 5))] [else (member (substr (hash-ref ca 'ca_zip) 0 5) zip_list)]))) (let* ([key (hash-ref s 's_store_name)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ss ss 'd d 's s 'ca ca 'c c) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (hash-ref g 'key))) (string>? (let ([g a]) (hash-ref g 'key)) (let ([g b]) (hash-ref g 'key)))] [(string? (let ([g b]) (hash-ref g 'key))) (string>? (let ([g a]) (hash-ref g 'key)) (let ([g b]) (hash-ref g 'key)))] [else (> (let ([g a]) (hash-ref g 'key)) (let ([g b]) (hash-ref g 'key)))]))))
  (for/list ([g _groups]) (hash 's_store_name (hash-ref g 'key) 'net_profit (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_net_profit))]) v))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 's_store_name "Store1" 'net_profit 10))) (displayln "ok"))
