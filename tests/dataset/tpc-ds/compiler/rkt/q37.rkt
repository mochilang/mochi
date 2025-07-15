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

(define item (list (hash 'i_item_sk 1 'i_item_id "I1" 'i_item_desc "Item1" 'i_current_price 30 'i_manufact_id 800) (hash 'i_item_sk 2 'i_item_id "I2" 'i_item_desc "Item2" 'i_current_price 60 'i_manufact_id 801)))
(define inventory (list (hash 'inv_item_sk 1 'inv_warehouse_sk 1 'inv_date_sk 1 'inv_quantity_on_hand 200) (hash 'inv_item_sk 2 'inv_warehouse_sk 1 'inv_date_sk 1 'inv_quantity_on_hand 300)))
(define date_dim (list (hash 'd_date_sk 1 'd_date "2000-01-15")))
(define catalog_sales (list (hash 'cs_item_sk 1 'cs_sold_date_sk 1)))
(define result (let ([groups (make-hash)])
  (for* ([i item] [inv inventory] [d date_dim] [cs catalog_sales] #:when (and (equal? (hash-ref i 'i_item_sk) (hash-ref inv 'inv_item_sk)) (equal? (hash-ref inv 'inv_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref cs 'cs_item_sk) (hash-ref i 'i_item_sk)) (and (and (and (and (and (_ge (hash-ref i 'i_current_price) 20) (_le (hash-ref i 'i_current_price) 50)) (_ge (hash-ref i 'i_manufact_id) 800)) (_le (hash-ref i 'i_manufact_id) 803)) (_ge (hash-ref inv 'inv_quantity_on_hand) 100)) (_le (hash-ref inv 'inv_quantity_on_hand) 500)))) (let* ([key (hash 'id (hash-ref i 'i_item_id) 'desc (hash-ref i 'i_item_desc) 'price (hash-ref i 'i_current_price))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'i i 'inv inv 'd d 'cs cs) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (hash-ref (hash-ref g 'key) 'id))) (string>? (let ([g a]) (hash-ref (hash-ref g 'key) 'id)) (let ([g b]) (hash-ref (hash-ref g 'key) 'id)))] [(string? (let ([g b]) (hash-ref (hash-ref g 'key) 'id))) (string>? (let ([g a]) (hash-ref (hash-ref g 'key) 'id)) (let ([g b]) (hash-ref (hash-ref g 'key) 'id)))] [else (> (let ([g a]) (hash-ref (hash-ref g 'key) 'id)) (let ([g b]) (hash-ref (hash-ref g 'key) 'id)))]))))
  (for/list ([g _groups]) (hash 'i_item_id (hash-ref (hash-ref g 'key) 'id) 'i_item_desc (hash-ref (hash-ref g 'key) 'desc) 'i_current_price (hash-ref (hash-ref g 'key) 'price)))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_item_id "I1" 'i_item_desc "Item1" 'i_current_price 30))) (displayln "ok"))
