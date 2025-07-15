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

(define store_sales (list (hash 'ss_cdemo_sk 1 'ss_sold_date_sk 1 'ss_item_sk 1 'ss_promo_sk 1 'ss_quantity 5 'ss_list_price 10 'ss_coupon_amt 2 'ss_sales_price 8)))
(define customer_demographics (list (hash 'cd_demo_sk 1 'cd_gender "M" 'cd_marital_status "S" 'cd_education_status "College")))
(define date_dim (list (hash 'd_date_sk 1 'd_year 1998)))
(define item (list (hash 'i_item_sk 1 'i_item_id "I1")))
(define promotion (list (hash 'p_promo_sk 1 'p_channel_email "N" 'p_channel_event "Y")))
(define result (let ([groups (make-hash)])
  (for* ([ss store_sales] [cd customer_demographics] [d date_dim] [i item] [p promotion] #:when (and (equal? (hash-ref ss 'ss_cdemo_sk) (hash-ref cd 'cd_demo_sk)) (equal? (hash-ref ss 'ss_sold_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref ss 'ss_item_sk) (hash-ref i 'i_item_sk)) (equal? (hash-ref ss 'ss_promo_sk) (hash-ref p 'p_promo_sk)) (and (and (and (and (string=? (hash-ref cd 'cd_gender) "M") (string=? (hash-ref cd 'cd_marital_status) "S")) (string=? (hash-ref cd 'cd_education_status) "College")) (or (string=? (hash-ref p 'p_channel_email) "N") (string=? (hash-ref p 'p_channel_event) "N"))) (equal? (hash-ref d 'd_year) 1998)))) (let* ([key (hash 'i_item_id (hash-ref i 'i_item_id))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ss ss 'cd cd 'd d 'i i 'p p) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (hash-ref (hash-ref g 'key) 'i_item_id))) (string>? (let ([g a]) (hash-ref (hash-ref g 'key) 'i_item_id)) (let ([g b]) (hash-ref (hash-ref g 'key) 'i_item_id)))] [(string? (let ([g b]) (hash-ref (hash-ref g 'key) 'i_item_id))) (string>? (let ([g a]) (hash-ref (hash-ref g 'key) 'i_item_id)) (let ([g b]) (hash-ref (hash-ref g 'key) 'i_item_id)))] [else (> (let ([g a]) (hash-ref (hash-ref g 'key) 'i_item_id)) (let ([g b]) (hash-ref (hash-ref g 'key) 'i_item_id)))]))))
  (for/list ([g _groups]) (hash 'i_item_id (hash-ref (hash-ref g 'key) 'i_item_id) 'agg1 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_quantity))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_quantity)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg2 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_list_price))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_list_price)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg3 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_coupon_amt))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_coupon_amt)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'agg4 (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_sales_price))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_sales_price)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_item_id "I1" 'agg1 5 'agg2 10 'agg3 2 'agg4 8))) (displayln "ok"))
