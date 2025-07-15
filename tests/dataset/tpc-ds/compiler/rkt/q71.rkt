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

(define item (list (hash 'i_item_sk 1 'i_brand_id 10 'i_brand "BrandA" 'i_manager_id 1) (hash 'i_item_sk 2 'i_brand_id 20 'i_brand "BrandB" 'i_manager_id 1)))
(define time_dim (list (hash 't_time_sk 1 't_hour 8 't_minute 30 't_meal_time "breakfast") (hash 't_time_sk 2 't_hour 18 't_minute 0 't_meal_time "dinner") (hash 't_time_sk 3 't_hour 12 't_minute 0 't_meal_time "lunch")))
(define date_dim (list (hash 'd_date_sk 1 'd_moy 12 'd_year 1998)))
(define web_sales (list (hash 'ws_ext_sales_price 100 'ws_sold_date_sk 1 'ws_item_sk 1 'ws_sold_time_sk 1)))
(define catalog_sales (list (hash 'cs_ext_sales_price 200 'cs_sold_date_sk 1 'cs_item_sk 1 'cs_sold_time_sk 2)))
(define store_sales (list (hash 'ss_ext_sales_price 150 'ss_sold_date_sk 1 'ss_item_sk 2 'ss_sold_time_sk 1)))
(define month 12)
(define year 1998)
(define union_sales (concat (for*/list ([ws web_sales] [d date_dim] #:when (and (equal? (hash-ref d 'd_date_sk) (hash-ref ws 'ws_sold_date_sk)) (and (equal? (hash-ref d 'd_moy) month) (equal? (hash-ref d 'd_year) year)))) (hash 'ext_price (hash-ref ws 'ws_ext_sales_price) 'item_sk (hash-ref ws 'ws_item_sk) 'time_sk (hash-ref ws 'ws_sold_time_sk))) (for*/list ([cs catalog_sales] [d date_dim] #:when (and (equal? (hash-ref d 'd_date_sk) (hash-ref cs 'cs_sold_date_sk)) (and (equal? (hash-ref d 'd_moy) month) (equal? (hash-ref d 'd_year) year)))) (hash 'ext_price (hash-ref cs 'cs_ext_sales_price) 'item_sk (hash-ref cs 'cs_item_sk) 'time_sk (hash-ref cs 'cs_sold_time_sk))) (for*/list ([ss store_sales] [d date_dim] #:when (and (equal? (hash-ref d 'd_date_sk) (hash-ref ss 'ss_sold_date_sk)) (and (equal? (hash-ref d 'd_moy) month) (equal? (hash-ref d 'd_year) year)))) (hash 'ext_price (hash-ref ss 'ss_ext_sales_price) 'item_sk (hash-ref ss 'ss_item_sk) 'time_sk (hash-ref ss 'ss_sold_time_sk)))))
(define result (let ([groups (make-hash)])
  (for* ([i item] [s union_sales] [t time_dim] #:when (and (equal? (hash-ref s 'item_sk) (hash-ref i 'i_item_sk)) (equal? (hash-ref t 't_time_sk) (hash-ref s 'time_sk)) (and (equal? (hash-ref i 'i_manager_id) 1) (or (string=? (hash-ref t 't_meal_time) "breakfast") (string=? (hash-ref t 't_meal_time) "dinner"))))) (let* ([key (hash 'brand_id (hash-ref i 'i_brand_id) 'brand (hash-ref i 'i_brand) 't_hour (hash-ref t 't_hour) 't_minute (hash-ref t 't_minute))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'i i 's s 't t) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (list (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 's) 'ext_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id)))) (string>? (let ([g a]) (list (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 's) 'ext_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id))) (let ([g b]) (list (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 's) 'ext_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id))))] [(string? (let ([g b]) (list (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 's) 'ext_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id)))) (string>? (let ([g a]) (list (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 's) 'ext_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id))) (let ([g b]) (list (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 's) 'ext_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id))))] [else (> (let ([g a]) (list (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 's) 'ext_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id))) (let ([g b]) (list (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 's) 'ext_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id))))]))))
  (for/list ([g _groups]) (hash 'i_brand_id (hash-ref (hash-ref g 'key) 'brand_id) 'i_brand (hash-ref (hash-ref g 'key) 'brand) 't_hour (hash-ref (hash-ref g 'key) 't_hour) 't_minute (hash-ref (hash-ref g 'key) 't_minute) 'ext_price (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 's) 'ext_price))]) v))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_brand_id 10 'i_brand "BrandA" 't_hour 18 't_minute 0 'ext_price 200) (hash 'i_brand_id 20 'i_brand "BrandB" 't_hour 8 't_minute 30 'ext_price 150) (hash 'i_brand_id 10 'i_brand "BrandA" 't_hour 8 't_minute 30 'ext_price 100))) (displayln "ok"))
