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

(define store (list (hash 's_store_sk 1 's_state "CA" 's_county "Orange") (hash 's_store_sk 2 's_state "CA" 's_county "Orange") (hash 's_store_sk 3 's_state "TX" 's_county "Travis")))
(define date_dim (list (hash 'd_date_sk 1 'd_month_seq 1200) (hash 'd_date_sk 2 'd_month_seq 1201)))
(define store_sales (list (hash 'ss_sold_date_sk 1 'ss_store_sk 1 'ss_net_profit 10) (hash 'ss_sold_date_sk 1 'ss_store_sk 2 'ss_net_profit 5) (hash 'ss_sold_date_sk 2 'ss_store_sk 3 'ss_net_profit 20)))
(define dms 1200)
(define result (let ([groups (make-hash)])
  (for* ([ss store_sales] [d date_dim] [s store] #:when (and (equal? (hash-ref d 'd_date_sk) (hash-ref ss 'ss_sold_date_sk)) (equal? (hash-ref s 's_store_sk) (hash-ref ss 'ss_store_sk)) (and (_ge (hash-ref d 'd_month_seq) dms) (_le (hash-ref d 'd_month_seq) (+ dms 11))))) (let* ([key (hash 'state (hash-ref s 's_state) 'county (hash-ref s 's_county))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ss ss 'd d 's s) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'state) (hash-ref (hash-ref g 'key) 'county)))) (string>? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'state) (hash-ref (hash-ref g 'key) 'county))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'state) (hash-ref (hash-ref g 'key) 'county))))] [(string? (let ([g b]) (list (hash-ref (hash-ref g 'key) 'state) (hash-ref (hash-ref g 'key) 'county)))) (string>? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'state) (hash-ref (hash-ref g 'key) 'county))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'state) (hash-ref (hash-ref g 'key) 'county))))] [else (> (let ([g a]) (list (hash-ref (hash-ref g 'key) 'state) (hash-ref (hash-ref g 'key) 'county))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'state) (hash-ref (hash-ref g 'key) 'county))))]))))
  (for/list ([g _groups]) (hash 's_state (hash-ref (hash-ref g 'key) 'state) 's_county (hash-ref (hash-ref g 'key) 'county) 'total_sum (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_net_profit))]) v))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 's_state "CA" 's_county "Orange" 'total_sum 15) (hash 's_state "TX" 's_county "Travis" 'total_sum 20))) (displayln "ok"))
