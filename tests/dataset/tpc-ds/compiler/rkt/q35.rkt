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

(define customer (list (hash 'c_customer_sk 1 'c_current_addr_sk 1 'c_current_cdemo_sk 1) (hash 'c_customer_sk 2 'c_current_addr_sk 2 'c_current_cdemo_sk 2)))
(define customer_address (list (hash 'ca_address_sk 1 'ca_state "CA") (hash 'ca_address_sk 2 'ca_state "NY")))
(define customer_demographics (list (hash 'cd_demo_sk 1 'cd_gender "M" 'cd_marital_status "S" 'cd_dep_count 1 'cd_dep_employed_count 1 'cd_dep_college_count 0) (hash 'cd_demo_sk 2 'cd_gender "F" 'cd_marital_status "M" 'cd_dep_count 2 'cd_dep_employed_count 1 'cd_dep_college_count 1)))
(define store_sales (list (hash 'ss_customer_sk 1 'ss_sold_date_sk 1)))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2000 'd_qoy 1)))
(define purchased (for*/list ([ss store_sales] [d date_dim] #:when (and (equal? (hash-ref ss 'ss_sold_date_sk) (hash-ref d 'd_date_sk)) (and (equal? (hash-ref d 'd_year) 2000) (_lt (hash-ref d 'd_qoy) 4)))) (hash-ref ss 'ss_customer_sk)))
(define groups (let ([groups (make-hash)])
  (for* ([c customer] [ca customer_address] [cd customer_demographics] #:when (and (equal? (hash-ref c 'c_current_addr_sk) (hash-ref ca 'ca_address_sk)) (equal? (hash-ref c 'c_current_cdemo_sk) (hash-ref cd 'cd_demo_sk)) (cond [(string? purchased) (regexp-match? (regexp (hash-ref c 'c_customer_sk)) purchased)] [(hash? purchased) (hash-has-key? purchased (hash-ref c 'c_customer_sk))] [else (member (hash-ref c 'c_customer_sk) purchased)]))) (let* ([key (hash 'state (hash-ref ca 'ca_state) 'gender (hash-ref cd 'cd_gender) 'marital (hash-ref cd 'cd_marital_status) 'dep (hash-ref cd 'cd_dep_count) 'emp (hash-ref cd 'cd_dep_employed_count) 'col (hash-ref cd 'cd_dep_college_count))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'c c 'ca ca 'cd cd) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'ca_state (hash-ref (hash-ref g 'key) 'state) 'cd_gender (hash-ref (hash-ref g 'key) 'gender) 'cd_marital_status (hash-ref (hash-ref g 'key) 'marital) 'cd_dep_count (hash-ref (hash-ref g 'key) 'dep) 'cd_dep_employed_count (hash-ref (hash-ref g 'key) 'emp) 'cd_dep_college_count (hash-ref (hash-ref g 'key) 'col) 'cnt (length (hash-ref g 'items))))))
(displayln (jsexpr->string (_json-fix groups)))
(when (equal? groups (list (hash 'ca_state "CA" 'cd_gender "M" 'cd_marital_status "S" 'cd_dep_count 1 'cd_dep_employed_count 1 'cd_dep_college_count 0 'cnt 1))) (displayln "ok"))
