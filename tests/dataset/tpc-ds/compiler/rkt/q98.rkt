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

(struct StoreSale (ss_item_sk ss_sold_date_sk ss_ext_sales_price) #:transparent #:mutable)
(struct Item (i_item_sk i_item_id i_item_desc i_category i_class i_current_price) #:transparent #:mutable)
(struct DateDim (d_date_sk d_date) #:transparent #:mutable)
(define store_sales (list (hash 'ss_item_sk 1 'ss_sold_date_sk 1 'ss_ext_sales_price 50) (hash 'ss_item_sk 2 'ss_sold_date_sk 1 'ss_ext_sales_price 100)))
(define item (list (hash 'i_item_sk 1 'i_item_id "I1" 'i_item_desc "desc1" 'i_category "CatA" 'i_class "Class1" 'i_current_price 100) (hash 'i_item_sk 2 'i_item_id "I2" 'i_item_desc "desc2" 'i_category "CatB" 'i_class "Class1" 'i_current_price 200)))
(define date_dim (list (hash 'd_date_sk 1 'd_date "2000-02-01")))
(define grouped (let ([groups (make-hash)])
  (for* ([ss store_sales] [i item] [d date_dim] #:when (and (equal? (hash-ref ss 'ss_item_sk) (hash-ref i 'i_item_sk)) (equal? (hash-ref ss 'ss_sold_date_sk) (hash-ref d 'd_date_sk)))) (let* ([key (hash 'item_id (hash-ref i 'i_item_id) 'item_desc (hash-ref i 'i_item_desc) 'category (hash-ref i 'i_category) 'class (hash-ref i 'i_class) 'price (hash-ref i 'i_current_price))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ss ss 'i i 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'i_item_id (hash-ref (hash-ref g 'key) 'item_id) 'i_item_desc (hash-ref (hash-ref g 'key) 'item_desc) 'i_category (hash-ref (hash-ref g 'key) 'category) 'i_class (hash-ref (hash-ref g 'key) 'class) 'i_current_price (hash-ref (hash-ref g 'key) 'price) 'itemrevenue (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_ext_sales_price))]) v))))))
(define totals (let ([groups (make-hash)])
  (for* ([g grouped]) (let* ([key (hash-ref g 'i_class)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons g bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([cg _groups]) (hash 'class (hash-ref cg 'key) 'total (apply + (for*/list ([v (for*/list ([x (hash-ref cg 'items)]) (hash-ref x 'itemrevenue))]) v))))))
(define result (let ([_items0 (for*/list ([g grouped] [t totals] #:when (and (equal? (hash-ref g 'i_class) (hash-ref t 'class)))) (hash 'g g 't t))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((g (hash-ref a 'g)) (t (hash-ref a 't))) (list (hash-ref g 'i_category) (hash-ref g 'i_class) (hash-ref g 'i_item_id)))) (string<? (let ((g (hash-ref a 'g)) (t (hash-ref a 't))) (list (hash-ref g 'i_category) (hash-ref g 'i_class) (hash-ref g 'i_item_id))) (let ((g (hash-ref b 'g)) (t (hash-ref b 't))) (list (hash-ref g 'i_category) (hash-ref g 'i_class) (hash-ref g 'i_item_id))))] [(string? (let ((g (hash-ref b 'g)) (t (hash-ref b 't))) (list (hash-ref g 'i_category) (hash-ref g 'i_class) (hash-ref g 'i_item_id)))) (string<? (let ((g (hash-ref a 'g)) (t (hash-ref a 't))) (list (hash-ref g 'i_category) (hash-ref g 'i_class) (hash-ref g 'i_item_id))) (let ((g (hash-ref b 'g)) (t (hash-ref b 't))) (list (hash-ref g 'i_category) (hash-ref g 'i_class) (hash-ref g 'i_item_id))))] [else (< (let ((g (hash-ref a 'g)) (t (hash-ref a 't))) (list (hash-ref g 'i_category) (hash-ref g 'i_class) (hash-ref g 'i_item_id))) (let ((g (hash-ref b 'g)) (t (hash-ref b 't))) (list (hash-ref g 'i_category) (hash-ref g 'i_class) (hash-ref g 'i_item_id))))]))))
  (for/list ([item _items0]) (let ((g (hash-ref item 'g)) (t (hash-ref item 't))) (hash 'i_item_id (hash-ref g 'i_item_id) 'i_item_desc (hash-ref g 'i_item_desc) 'i_category (hash-ref g 'i_category) 'i_class (hash-ref g 'i_class) 'i_current_price (hash-ref g 'i_current_price) 'itemrevenue (hash-ref g 'itemrevenue) 'revenueratio (/ (* (hash-ref g 'itemrevenue) 100) (hash-ref t 'total)))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_item_id "I1" 'i_item_desc "desc1" 'i_category "CatA" 'i_class "Class1" 'i_current_price 100 'itemrevenue 50 'revenueratio 33.333333333333336) (hash 'i_item_id "I2" 'i_item_desc "desc2" 'i_category "CatB" 'i_class "Class1" 'i_current_price 200 'itemrevenue 100 'revenueratio 66.66666666666667))) (displayln "ok"))
