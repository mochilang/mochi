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

(define store_sales (list (hash 'sold_date_sk 1 'item_sk 1 'ext_sales_price 10) (hash 'sold_date_sk 1 'item_sk 2 'ext_sales_price 20) (hash 'sold_date_sk 2 'item_sk 1 'ext_sales_price 15)))
(define item (list (hash 'i_item_sk 1 'i_manager_id 1 'i_category_id 100 'i_category "CatA") (hash 'i_item_sk 2 'i_manager_id 1 'i_category_id 200 'i_category "CatB")))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2020 'd_moy 5) (hash 'd_date_sk 2 'd_year 2021 'd_moy 5)))
(define month 5)
(define year 2020)
(define records (for*/list ([dt date_dim] [ss store_sales] [it item] #:when (and (equal? (hash-ref ss 'sold_date_sk) (hash-ref dt 'd_date_sk)) (equal? (hash-ref ss 'item_sk) (hash-ref it 'i_item_sk)) (and (and (equal? (hash-ref it 'i_manager_id) 1) (equal? (hash-ref dt 'd_moy) month)) (equal? (hash-ref dt 'd_year) year)))) (hash 'd_year (hash-ref dt 'd_year) 'i_category_id (hash-ref it 'i_category_id) 'i_category (hash-ref it 'i_category) 'price (hash-ref ss 'ext_sales_price))))
(define grouped (let ([groups (make-hash)])
  (for* ([r records]) (let* ([key (hash 'd_year (hash-ref r 'd_year) 'i_category_id (hash-ref r 'i_category_id) 'i_category (hash-ref r 'i_category))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons r bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'd_year (hash-ref (hash-ref g 'key) 'd_year) 'i_category_id (hash-ref (hash-ref g 'key) 'i_category_id) 'i_category (hash-ref (hash-ref g 'key) 'i_category) 'sum_ss_ext_sales_price (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v))))))
(define base (let ([_items0 (for*/list ([g grouped]) (hash 'g g))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((g (hash-ref a 'g))) (hash-ref g 'sum_ss_ext_sales_price))) (string>? (let ((g (hash-ref a 'g))) (hash-ref g 'sum_ss_ext_sales_price)) (let ((g (hash-ref b 'g))) (hash-ref g 'sum_ss_ext_sales_price)))] [(string? (let ((g (hash-ref b 'g))) (hash-ref g 'sum_ss_ext_sales_price))) (string>? (let ((g (hash-ref a 'g))) (hash-ref g 'sum_ss_ext_sales_price)) (let ((g (hash-ref b 'g))) (hash-ref g 'sum_ss_ext_sales_price)))] [else (> (let ((g (hash-ref a 'g))) (hash-ref g 'sum_ss_ext_sales_price)) (let ((g (hash-ref b 'g))) (hash-ref g 'sum_ss_ext_sales_price)))]))))
  (for/list ([item _items0]) (let ((g (hash-ref item 'g))) g))))
(define result base)
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'd_year 2020 'i_category_id 200 'i_category "CatB" 'sum_ss_ext_sales_price 20) (hash 'd_year 2020 'i_category_id 100 'i_category "CatA" 'sum_ss_ext_sales_price 10))) (displayln "ok"))
