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

(define time_dim (list (hash 'time_sk 1 'hour 8 'minute 30) (hash 'time_sk 2 'hour 9 'minute 0) (hash 'time_sk 3 'hour 9 'minute 30) (hash 'time_sk 4 'hour 10 'minute 0) (hash 'time_sk 5 'hour 10 'minute 30) (hash 'time_sk 6 'hour 11 'minute 0) (hash 'time_sk 7 'hour 11 'minute 30) (hash 'time_sk 8 'hour 12 'minute 0)))
(define household_demographics (list (hash 'hd_demo_sk 1 'hd_dep_count 1 'hd_vehicle_count 1)))
(define store (list (hash 's_store_sk 1 's_store_name "ese")))
(define store_sales (list (hash 'sold_time_sk 1 'hdemo_sk 1 'store_sk 1 'qty 10) (hash 'sold_time_sk 2 'hdemo_sk 1 'store_sk 1 'qty 12) (hash 'sold_time_sk 3 'hdemo_sk 1 'store_sk 1 'qty 14) (hash 'sold_time_sk 4 'hdemo_sk 1 'store_sk 1 'qty 11) (hash 'sold_time_sk 5 'hdemo_sk 1 'store_sk 1 'qty 8) (hash 'sold_time_sk 6 'hdemo_sk 1 'store_sk 1 'qty 9) (hash 'sold_time_sk 7 'hdemo_sk 1 'store_sk 1 'qty 10) (hash 'sold_time_sk 8 'hdemo_sk 1 'store_sk 1 'qty 14)))
(define (count_range ssales tdim hour start_min end_min)
  (let/ec return
(define total 0)
(for ([ss (if (hash? ssales) (hash-keys ssales) ssales)])
(for ([t (if (hash? tdim) (hash-keys tdim) tdim)])
(if (and (and (and (equal? (hash-ref ss 'sold_time_sk) (hash-ref t 'time_sk)) (equal? (hash-ref t 'hour) hour)) (_ge (hash-ref t 'minute) start_min)) (_lt (hash-ref t 'minute) end_min))
  (begin
(set! total (+ total (hash-ref ss 'qty)))
  )
  (void)
)
)
)
(return total)
  ))
(define h8_30_to_9 (count_range store_sales time_dim 8 30 60))
(define h9_to_9_30 (count_range store_sales time_dim 9 0 30))
(define h9_30_to_10 (count_range store_sales time_dim 9 30 60))
(define h10_to_10_30 (count_range store_sales time_dim 10 0 30))
(define h10_30_to_11 (count_range store_sales time_dim 10 30 60))
(define h11_to_11_30 (count_range store_sales time_dim 11 0 30))
(define h11_30_to_12 (count_range store_sales time_dim 11 30 60))
(define h12_to_12_30 (count_range store_sales time_dim 12 0 30))
(define result (+ (+ (+ (+ (+ (+ (+ h8_30_to_9 h9_to_9_30) h9_30_to_10) h10_to_10_30) h10_30_to_11) h11_to_11_30) h11_30_to_12) h12_to_12_30))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result 88) (displayln "ok"))
