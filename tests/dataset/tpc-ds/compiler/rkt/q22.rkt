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

(struct Inventory (inv_item_sk inv_date_sk inv_quantity_on_hand) #:transparent #:mutable)
(struct DateDim (d_date_sk d_month_seq) #:transparent #:mutable)
(struct Item (i_item_sk i_product_name i_brand i_class i_category) #:transparent #:mutable)
(define inventory (list (hash 'inv_item_sk 1 'inv_date_sk 1 'inv_quantity_on_hand 10) (hash 'inv_item_sk 1 'inv_date_sk 2 'inv_quantity_on_hand 20) (hash 'inv_item_sk 1 'inv_date_sk 3 'inv_quantity_on_hand 10) (hash 'inv_item_sk 1 'inv_date_sk 4 'inv_quantity_on_hand 20) (hash 'inv_item_sk 2 'inv_date_sk 1 'inv_quantity_on_hand 50)))
(define date_dim (list (hash 'd_date_sk 1 'd_month_seq 0) (hash 'd_date_sk 2 'd_month_seq 1) (hash 'd_date_sk 3 'd_month_seq 2) (hash 'd_date_sk 4 'd_month_seq 3)))
(define item (list (hash 'i_item_sk 1 'i_product_name "Prod1" 'i_brand "Brand1" 'i_class "Class1" 'i_category "Cat1") (hash 'i_item_sk 2 'i_product_name "Prod2" 'i_brand "Brand2" 'i_class "Class2" 'i_category "Cat2")))
(define qoh (let ([groups (make-hash)])
  (for* ([inv inventory] [d date_dim] [i item] #:when (and (equal? (hash-ref inv 'inv_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref inv 'inv_item_sk) (hash-ref i 'i_item_sk)) (and (_ge (hash-ref d 'd_month_seq) 0) (_le (hash-ref d 'd_month_seq) 11)))) (let* ([key (hash 'product_name (hash-ref i 'i_product_name) 'brand (hash-ref i 'i_brand) 'class (hash-ref i 'i_class) 'category (hash-ref i 'i_category))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'inv inv 'd d 'i i) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'i_product_name (hash-ref (hash-ref g 'key) 'product_name) 'i_brand (hash-ref (hash-ref g 'key) 'brand) 'i_class (hash-ref (hash-ref g 'key) 'class) 'i_category (hash-ref (hash-ref g 'key) 'category) 'qoh (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'inv_quantity_on_hand))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'inv_quantity_on_hand)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))))
(displayln (jsexpr->string (_json-fix qoh)))
(when (equal? qoh (list (hash 'i_product_name "Prod1" 'i_brand "Brand1" 'i_class "Class1" 'i_category "Cat1" 'qoh 15) (hash 'i_product_name "Prod2" 'i_brand "Brand2" 'i_class "Class2" 'i_category "Cat2" 'qoh 50))) (displayln "ok"))
