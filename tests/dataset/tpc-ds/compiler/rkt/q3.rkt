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

(define date_dim (list (hash 'd_date_sk 1 'd_year 1998 'd_moy 12)))
(define store_sales (list (hash 'ss_sold_date_sk 1 'ss_item_sk 1 'ss_ext_sales_price 10) (hash 'ss_sold_date_sk 1 'ss_item_sk 2 'ss_ext_sales_price 20)))
(define item (list (hash 'i_item_sk 1 'i_manufact_id 100 'i_brand_id 1 'i_brand "Brand1") (hash 'i_item_sk 2 'i_manufact_id 100 'i_brand_id 2 'i_brand "Brand2")))
(define result (let ([groups (make-hash)])
  (for* ([dt date_dim] [ss store_sales] [i item] #:when (and (equal? (hash-ref dt 'd_date_sk) (hash-ref ss 'ss_sold_date_sk)) (equal? (hash-ref ss 'ss_item_sk) (hash-ref i 'i_item_sk)) (and (equal? (hash-ref i 'i_manufact_id) 100) (equal? (hash-ref dt 'd_moy) 12)))) (let* ([key (hash 'd_year (hash-ref dt 'd_year) 'brand_id (hash-ref i 'i_brand_id) 'brand (hash-ref i 'i_brand))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'dt dt 'ss ss 'i i) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'd_year) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_sales_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id)))) (string>? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'd_year) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_sales_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'd_year) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_sales_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id))))] [(string? (let ([g b]) (list (hash-ref (hash-ref g 'key) 'd_year) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_sales_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id)))) (string>? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'd_year) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_sales_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'd_year) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_sales_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id))))] [else (> (let ([g a]) (list (hash-ref (hash-ref g 'key) 'd_year) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_sales_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'd_year) (- (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_sales_price))]) v))) (hash-ref (hash-ref g 'key) 'brand_id))))]))))
  (for/list ([g _groups]) (hash 'd_year (hash-ref (hash-ref g 'key) 'd_year) 'brand_id (hash-ref (hash-ref g 'key) 'brand_id) 'brand (hash-ref (hash-ref g 'key) 'brand) 'sum_agg (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_sales_price))]) v))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'd_year 1998 'brand_id 2 'brand "Brand2" 'sum_agg 20) (hash 'd_year 1998 'brand_id 1 'brand "Brand1" 'sum_agg 10))) (displayln "ok"))
