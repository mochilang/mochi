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

(define web_sales (list (hash 'ws_sold_date_sk 1 'ws_ext_sales_price 5 'ws_sold_date_name "Sunday") (hash 'ws_sold_date_sk 2 'ws_ext_sales_price 5 'ws_sold_date_name "Monday") (hash 'ws_sold_date_sk 8 'ws_ext_sales_price 10 'ws_sold_date_name "Sunday") (hash 'ws_sold_date_sk 9 'ws_ext_sales_price 10 'ws_sold_date_name "Monday")))
(define catalog_sales (list (hash 'cs_sold_date_sk 1 'cs_ext_sales_price 5 'cs_sold_date_name "Sunday") (hash 'cs_sold_date_sk 2 'cs_ext_sales_price 5 'cs_sold_date_name "Monday") (hash 'cs_sold_date_sk 8 'cs_ext_sales_price 10 'cs_sold_date_name "Sunday") (hash 'cs_sold_date_sk 9 'cs_ext_sales_price 10 'cs_sold_date_name "Monday")))
(define date_dim (list (hash 'd_date_sk 1 'd_week_seq 1 'd_day_name "Sunday" 'd_year 1998) (hash 'd_date_sk 2 'd_week_seq 1 'd_day_name "Monday" 'd_year 1998) (hash 'd_date_sk 8 'd_week_seq 54 'd_day_name "Sunday" 'd_year 1999) (hash 'd_date_sk 9 'd_week_seq 54 'd_day_name "Monday" 'd_year 1999)))
(define wscs (append (for*/list ([ws web_sales]) (hash 'sold_date_sk (hash-ref ws 'ws_sold_date_sk) 'sales_price (hash-ref ws 'ws_ext_sales_price) 'day (hash-ref ws 'ws_sold_date_name))) (for*/list ([cs catalog_sales]) (hash 'sold_date_sk (hash-ref cs 'cs_sold_date_sk) 'sales_price (hash-ref cs 'cs_ext_sales_price) 'day (hash-ref cs 'cs_sold_date_name)))))
(define wswscs (let ([groups (make-hash)])
  (for* ([w wscs] [d date_dim] #:when (and (equal? (hash-ref w 'sold_date_sk) (hash-ref d 'd_date_sk)))) (let* ([key (hash 'week_seq (hash-ref d 'd_week_seq))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'w w 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'd_week_seq (hash-ref (hash-ref g 'key) 'week_seq) 'sun_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)] #:when (and (string=? (hash-ref x 'day) "Sunday"))) (hash-ref x 'sales_price))]) v)) 'mon_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)] #:when (and (string=? (hash-ref x 'day) "Monday"))) (hash-ref x 'sales_price))]) v)) 'tue_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)] #:when (and (string=? (hash-ref x 'day) "Tuesday"))) (hash-ref x 'sales_price))]) v)) 'wed_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)] #:when (and (string=? (hash-ref x 'day) "Wednesday"))) (hash-ref x 'sales_price))]) v)) 'thu_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)] #:when (and (string=? (hash-ref x 'day) "Thursday"))) (hash-ref x 'sales_price))]) v)) 'fri_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)] #:when (and (string=? (hash-ref x 'day) "Friday"))) (hash-ref x 'sales_price))]) v)) 'sat_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)] #:when (and (string=? (hash-ref x 'day) "Saturday"))) (hash-ref x 'sales_price))]) v))))))
(define year1 (for*/list ([w wswscs] #:when (and (equal? (hash-ref w 'd_week_seq) 1))) w))
(define year2 (for*/list ([w wswscs] #:when (and (equal? (hash-ref w 'd_week_seq) 54))) w))
(define result (for*/list ([y year1] [z year2] #:when (and (equal? (hash-ref y 'd_week_seq) (- (hash-ref z 'd_week_seq) 53)))) (hash 'd_week_seq1 (hash-ref y 'd_week_seq) 'sun_ratio (/ (hash-ref y 'sun_sales) (hash-ref z 'sun_sales)) 'mon_ratio (/ (hash-ref y 'mon_sales) (hash-ref z 'mon_sales)))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'd_week_seq1 1 'sun_ratio 0.5 'mon_ratio 0.5))) (displayln "ok"))
