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

(define customer (list (hash 'id 1 'current_addr 1 'cdemo 1 'hdemo 1) (hash 'id 2 'current_addr 1 'cdemo 2 'hdemo 2) (hash 'id 3 'current_addr 1 'cdemo 3 'hdemo 1) (hash 'id 4 'current_addr 1 'cdemo 4 'hdemo 2)))
(define customer_address (list (hash 'ca_address_sk 1 'ca_city "Springfield")))
(define customer_demographics (list (hash 'cd_demo_sk 1) (hash 'cd_demo_sk 2) (hash 'cd_demo_sk 3) (hash 'cd_demo_sk 4)))
(define household_demographics (list (hash 'hd_demo_sk 1 'income_band_sk 1) (hash 'hd_demo_sk 2 'income_band_sk 1)))
(define income_band (list (hash 'ib_income_band_sk 1 'ib_lower_bound 0 'ib_upper_bound 50000)))
(define store_returns (list (hash 'sr_cdemo_sk 1 'amt 10) (hash 'sr_cdemo_sk 2 'amt 20) (hash 'sr_cdemo_sk 3 'amt 30) (hash 'sr_cdemo_sk 4 'amt 24)))
(define result (apply + (for*/list ([v (for*/list ([c customer] [ca customer_address] [cd customer_demographics] [sr store_returns] [hd household_demographics] [ib income_band] #:when (and (and (equal? (hash-ref c 'current_addr) (hash-ref ca 'ca_address_sk)) (string=? (hash-ref ca 'ca_city) "Springfield")) (equal? (hash-ref c 'cdemo) (hash-ref cd 'cd_demo_sk)) (equal? (hash-ref cd 'cd_demo_sk) (hash-ref sr 'sr_cdemo_sk)) (equal? (hash-ref c 'hdemo) (hash-ref hd 'hd_demo_sk)) (equal? (hash-ref hd 'income_band_sk) (hash-ref ib 'ib_income_band_sk)))) (hash-ref sr 'amt))]) v)))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result 84) (displayln "ok"))
