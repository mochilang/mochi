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

(struct StoreSale (ss_sold_time_sk ss_hdemo_sk ss_store_sk) #:transparent #:mutable)
(struct HouseholdDemographics (hd_demo_sk hd_dep_count) #:transparent #:mutable)
(struct TimeDim (t_time_sk t_hour t_minute) #:transparent #:mutable)
(struct Store (s_store_sk s_store_name) #:transparent #:mutable)
(define store_sales (list (hash 'ss_sold_time_sk 1 'ss_hdemo_sk 1 'ss_store_sk 1) (hash 'ss_sold_time_sk 1 'ss_hdemo_sk 1 'ss_store_sk 1) (hash 'ss_sold_time_sk 2 'ss_hdemo_sk 1 'ss_store_sk 1)))
(define household_demographics (list (hash 'hd_demo_sk 1 'hd_dep_count 3)))
(define time_dim (list (hash 't_time_sk 1 't_hour 20 't_minute 35) (hash 't_time_sk 2 't_hour 20 't_minute 45)))
(define store (list (hash 's_store_sk 1 's_store_name "ese")))
(define result (if (and (hash? (for*/list ([ss store_sales] [hd household_demographics] [t time_dim] [s store] #:when (and (equal? (hash-ref ss 'ss_hdemo_sk) (hash-ref hd 'hd_demo_sk)) (equal? (hash-ref ss 'ss_sold_time_sk) (hash-ref t 't_time_sk)) (equal? (hash-ref ss 'ss_store_sk) (hash-ref s 's_store_sk)) (and (and (and (equal? (hash-ref t 't_hour) 20) (_ge (hash-ref t 't_minute) 30)) (equal? (hash-ref hd 'hd_dep_count) 3)) (string=? (hash-ref s 's_store_name) "ese")))) ss)) (hash-has-key? (for*/list ([ss store_sales] [hd household_demographics] [t time_dim] [s store] #:when (and (equal? (hash-ref ss 'ss_hdemo_sk) (hash-ref hd 'hd_demo_sk)) (equal? (hash-ref ss 'ss_sold_time_sk) (hash-ref t 't_time_sk)) (equal? (hash-ref ss 'ss_store_sk) (hash-ref s 's_store_sk)) (and (and (and (equal? (hash-ref t 't_hour) 20) (_ge (hash-ref t 't_minute) 30)) (equal? (hash-ref hd 'hd_dep_count) 3)) (string=? (hash-ref s 's_store_name) "ese")))) ss) 'items)) (length (hash-ref (for*/list ([ss store_sales] [hd household_demographics] [t time_dim] [s store] #:when (and (equal? (hash-ref ss 'ss_hdemo_sk) (hash-ref hd 'hd_demo_sk)) (equal? (hash-ref ss 'ss_sold_time_sk) (hash-ref t 't_time_sk)) (equal? (hash-ref ss 'ss_store_sk) (hash-ref s 's_store_sk)) (and (and (and (equal? (hash-ref t 't_hour) 20) (_ge (hash-ref t 't_minute) 30)) (equal? (hash-ref hd 'hd_dep_count) 3)) (string=? (hash-ref s 's_store_name) "ese")))) ss) 'items)) (length (for*/list ([ss store_sales] [hd household_demographics] [t time_dim] [s store] #:when (and (equal? (hash-ref ss 'ss_hdemo_sk) (hash-ref hd 'hd_demo_sk)) (equal? (hash-ref ss 'ss_sold_time_sk) (hash-ref t 't_time_sk)) (equal? (hash-ref ss 'ss_store_sk) (hash-ref s 's_store_sk)) (and (and (and (equal? (hash-ref t 't_hour) 20) (_ge (hash-ref t 't_minute) 30)) (equal? (hash-ref hd 'hd_dep_count) 3)) (string=? (hash-ref s 's_store_name) "ese")))) ss))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result 3) (displayln "ok"))
