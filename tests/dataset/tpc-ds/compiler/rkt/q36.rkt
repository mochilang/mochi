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

(define store_sales (list (hash 'ss_item_sk 1 'ss_store_sk 1 'ss_sold_date_sk 1 'ss_ext_sales_price 100 'ss_net_profit 20) (hash 'ss_item_sk 2 'ss_store_sk 1 'ss_sold_date_sk 1 'ss_ext_sales_price 200 'ss_net_profit 50) (hash 'ss_item_sk 3 'ss_store_sk 2 'ss_sold_date_sk 1 'ss_ext_sales_price 150 'ss_net_profit 30)))
(define item (list (hash 'i_item_sk 1 'i_category "Books" 'i_class "C1") (hash 'i_item_sk 2 'i_category "Books" 'i_class "C2") (hash 'i_item_sk 3 'i_category "Electronics" 'i_class "C3")))
(define store (list (hash 's_store_sk 1 's_state "A") (hash 's_store_sk 2 's_state "B")))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2000)))
(define result (let ([groups (make-hash)])
  (for* ([ss store_sales] [d date_dim] [i item] [s store] #:when (and (equal? (hash-ref ss 'ss_sold_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref ss 'ss_item_sk) (hash-ref i 'i_item_sk)) (equal? (hash-ref ss 'ss_store_sk) (hash-ref s 's_store_sk)) (and (equal? (hash-ref d 'd_year) 2000) (or (string=? (hash-ref s 's_state) "A") (string=? (hash-ref s 's_state) "B"))))) (let* ([key (hash 'category (hash-ref i 'i_category) 'class (hash-ref i 'i_class))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ss ss 'd d 'i i 's s) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'category) (hash-ref (hash-ref g 'key) 'class)))) (string>? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'category) (hash-ref (hash-ref g 'key) 'class))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'category) (hash-ref (hash-ref g 'key) 'class))))] [(string? (let ([g b]) (list (hash-ref (hash-ref g 'key) 'category) (hash-ref (hash-ref g 'key) 'class)))) (string>? (let ([g a]) (list (hash-ref (hash-ref g 'key) 'category) (hash-ref (hash-ref g 'key) 'class))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'category) (hash-ref (hash-ref g 'key) 'class))))] [else (> (let ([g a]) (list (hash-ref (hash-ref g 'key) 'category) (hash-ref (hash-ref g 'key) 'class))) (let ([g b]) (list (hash-ref (hash-ref g 'key) 'category) (hash-ref (hash-ref g 'key) 'class))))]))))
  (for/list ([g _groups]) (hash 'i_category (hash-ref (hash-ref g 'key) 'category) 'i_class (hash-ref (hash-ref g 'key) 'class) 'gross_margin (/ (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_net_profit))]) v)) (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_sales_price))]) v)))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_category "Books" 'i_class "C1" 'gross_margin 0.2) (hash 'i_category "Books" 'i_class "C2" 'gross_margin 0.25) (hash 'i_category "Electronics" 'i_class "C3" 'gross_margin 0.2))) (displayln "ok"))
