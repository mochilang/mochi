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

(define date_dim (list (hash 'date_sk 1 'd_day_name "Sunday" 'd_year 2020) (hash 'date_sk 2 'd_day_name "Monday" 'd_year 2020) (hash 'date_sk 3 'd_day_name "Tuesday" 'd_year 2020) (hash 'date_sk 4 'd_day_name "Wednesday" 'd_year 2020) (hash 'date_sk 5 'd_day_name "Thursday" 'd_year 2020) (hash 'date_sk 6 'd_day_name "Friday" 'd_year 2020) (hash 'date_sk 7 'd_day_name "Saturday" 'd_year 2020)))
(define store (list (hash 'store_sk 1 'store_id "S1" 'store_name "Main" 'gmt_offset 0)))
(define store_sales (list (hash 'sold_date_sk 1 'store_sk 1 'sales_price 10) (hash 'sold_date_sk 2 'store_sk 1 'sales_price 20) (hash 'sold_date_sk 3 'store_sk 1 'sales_price 30) (hash 'sold_date_sk 4 'store_sk 1 'sales_price 40) (hash 'sold_date_sk 5 'store_sk 1 'sales_price 50) (hash 'sold_date_sk 6 'store_sk 1 'sales_price 60) (hash 'sold_date_sk 7 'store_sk 1 'sales_price 70)))
(define year 2020)
(define gmt 0)
(define records (for*/list ([d date_dim] [ss store_sales] [s store] #:when (and (equal? (hash-ref ss 'sold_date_sk) (hash-ref d 'date_sk)) (equal? (hash-ref ss 'store_sk) (hash-ref s 'store_sk)) (and (equal? (hash-ref s 'gmt_offset) gmt) (equal? (hash-ref d 'd_year) year)))) (hash 'd_day_name (hash-ref d 'd_day_name) 's_store_name (hash-ref s 'store_name) 's_store_id (hash-ref s 'store_id) 'price (hash-ref ss 'sales_price))))
(define base (let ([groups (make-hash)])
  (for* ([r records]) (let* ([key (hash 'name (hash-ref r 's_store_name) 'id (hash-ref r 's_store_id))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons r bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 's_store_name (hash-ref (hash-ref g 'key) 'name) 's_store_id (hash-ref (hash-ref g 'key) 'id) 'sun_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (if (string=? (hash-ref x 'd_day_name) "Sunday") (hash-ref x 'price) 0))]) v)) 'mon_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (if (string=? (hash-ref x 'd_day_name) "Monday") (hash-ref x 'price) 0))]) v)) 'tue_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (if (string=? (hash-ref x 'd_day_name) "Tuesday") (hash-ref x 'price) 0))]) v)) 'wed_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (if (string=? (hash-ref x 'd_day_name) "Wednesday") (hash-ref x 'price) 0))]) v)) 'thu_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (if (string=? (hash-ref x 'd_day_name) "Thursday") (hash-ref x 'price) 0))]) v)) 'fri_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (if (string=? (hash-ref x 'd_day_name) "Friday") (hash-ref x 'price) 0))]) v)) 'sat_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (if (string=? (hash-ref x 'd_day_name) "Saturday") (hash-ref x 'price) 0))]) v))))))
(define result base)
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 's_store_name "Main" 's_store_id "S1" 'sun_sales 10 'mon_sales 20 'tue_sales 30 'wed_sales 40 'thu_sales 50 'fri_sales 60 'sat_sales 70))) (displayln "ok"))
