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

(struct CatalogSale (cs_item_sk cs_sold_date_sk cs_ext_sales_price) #:transparent #:mutable)
(struct Item (i_item_sk i_item_id i_item_desc i_category i_class i_current_price) #:transparent #:mutable)
(struct DateDim (d_date_sk d_date) #:transparent #:mutable)
(define catalog_sales (list (hash 'cs_item_sk 1 'cs_sold_date_sk 1 'cs_ext_sales_price 100) (hash 'cs_item_sk 1 'cs_sold_date_sk 1 'cs_ext_sales_price 200) (hash 'cs_item_sk 2 'cs_sold_date_sk 1 'cs_ext_sales_price 150) (hash 'cs_item_sk 1 'cs_sold_date_sk 2 'cs_ext_sales_price 300) (hash 'cs_item_sk 2 'cs_sold_date_sk 2 'cs_ext_sales_price 150) (hash 'cs_item_sk 3 'cs_sold_date_sk 1 'cs_ext_sales_price 50)))
(define item (list (hash 'i_item_sk 1 'i_item_id "ITEM1" 'i_item_desc "Item One" 'i_category "A" 'i_class "X" 'i_current_price 10) (hash 'i_item_sk 2 'i_item_id "ITEM2" 'i_item_desc "Item Two" 'i_category "A" 'i_class "X" 'i_current_price 20) (hash 'i_item_sk 3 'i_item_id "ITEM3" 'i_item_desc "Item Three" 'i_category "D" 'i_class "Y" 'i_current_price 15)))
(define date_dim (list (hash 'd_date_sk 1 'd_date "2000-02-10") (hash 'd_date_sk 2 'd_date "2000-02-20")))
(define filtered (let ([groups (make-hash)])
  (for* ([cs catalog_sales] [i item] [d date_dim] #:when (and (equal? (hash-ref cs 'cs_item_sk) (hash-ref i 'i_item_sk)) (equal? (hash-ref cs 'cs_sold_date_sk) (hash-ref d 'd_date_sk)) (and (and (cond [(string? '("A" "B" "C")) (regexp-match? (regexp (hash-ref i 'i_category)) '("A" "B" "C"))] [(hash? '("A" "B" "C")) (hash-has-key? '("A" "B" "C") (hash-ref i 'i_category))] [else (member (hash-ref i 'i_category) '("A" "B" "C"))]) (string>=? (hash-ref d 'd_date) "2000-02-01")) (string<=? (hash-ref d 'd_date) "2000-03-02")))) (let* ([key (hash 'id (hash-ref i 'i_item_id) 'desc (hash-ref i 'i_item_desc) 'cat (hash-ref i 'i_category) 'class (hash-ref i 'i_class) 'price (hash-ref i 'i_current_price))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'cs cs 'i i 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'i_item_id (hash-ref (hash-ref g 'key) 'id) 'i_item_desc (hash-ref (hash-ref g 'key) 'desc) 'i_category (hash-ref (hash-ref g 'key) 'cat) 'i_class (hash-ref (hash-ref g 'key) 'class) 'i_current_price (hash-ref (hash-ref g 'key) 'price) 'itemrevenue (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_ext_sales_price))]) v))))))
(define class_totals (let ([groups (make-hash)])
  (for* ([f filtered]) (let* ([key (hash-ref f 'i_class)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons f bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'class (hash-ref g 'key) 'total (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'itemrevenue))]) v))))))
(define result (let ([_items0 (for*/list ([f filtered] [t class_totals] #:when (and (equal? (hash-ref f 'i_class) (hash-ref t 'class)))) (hash 'f f 't t))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((f (hash-ref a 'f)) (t (hash-ref a 't))) (list (hash-ref f 'i_category) (hash-ref f 'i_class) (hash-ref f 'i_item_id) (hash-ref f 'i_item_desc)))) (string<? (let ((f (hash-ref a 'f)) (t (hash-ref a 't))) (list (hash-ref f 'i_category) (hash-ref f 'i_class) (hash-ref f 'i_item_id) (hash-ref f 'i_item_desc))) (let ((f (hash-ref b 'f)) (t (hash-ref b 't))) (list (hash-ref f 'i_category) (hash-ref f 'i_class) (hash-ref f 'i_item_id) (hash-ref f 'i_item_desc))))] [(string? (let ((f (hash-ref b 'f)) (t (hash-ref b 't))) (list (hash-ref f 'i_category) (hash-ref f 'i_class) (hash-ref f 'i_item_id) (hash-ref f 'i_item_desc)))) (string<? (let ((f (hash-ref a 'f)) (t (hash-ref a 't))) (list (hash-ref f 'i_category) (hash-ref f 'i_class) (hash-ref f 'i_item_id) (hash-ref f 'i_item_desc))) (let ((f (hash-ref b 'f)) (t (hash-ref b 't))) (list (hash-ref f 'i_category) (hash-ref f 'i_class) (hash-ref f 'i_item_id) (hash-ref f 'i_item_desc))))] [else (< (let ((f (hash-ref a 'f)) (t (hash-ref a 't))) (list (hash-ref f 'i_category) (hash-ref f 'i_class) (hash-ref f 'i_item_id) (hash-ref f 'i_item_desc))) (let ((f (hash-ref b 'f)) (t (hash-ref b 't))) (list (hash-ref f 'i_category) (hash-ref f 'i_class) (hash-ref f 'i_item_id) (hash-ref f 'i_item_desc))))]))))
  (for/list ([item _items0]) (let ((f (hash-ref item 'f)) (t (hash-ref item 't))) (hash 'i_item_id (hash-ref f 'i_item_id) 'i_item_desc (hash-ref f 'i_item_desc) 'i_category (hash-ref f 'i_category) 'i_class (hash-ref f 'i_class) 'i_current_price (hash-ref f 'i_current_price) 'itemrevenue (hash-ref f 'itemrevenue) 'revenueratio (/ (* (hash-ref f 'itemrevenue) 100) (hash-ref t 'total)))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_item_id "ITEM1" 'i_item_desc "Item One" 'i_category "A" 'i_class "X" 'i_current_price 10 'itemrevenue 600 'revenueratio 66.66666666666667) (hash 'i_item_id "ITEM2" 'i_item_desc "Item Two" 'i_category "A" 'i_class "X" 'i_current_price 20 'itemrevenue 300 'revenueratio 33.333333333333336))) (displayln "ok"))
