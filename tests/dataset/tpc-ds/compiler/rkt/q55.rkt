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

(define store_sales (list (hash 'item 1 'sold_date 1 'price 10) (hash 'item 2 'sold_date 1 'price 20) (hash 'item 3 'sold_date 1 'price 25)))
(define item (list (hash 'i_item_sk 1 'i_brand_id 10 'i_manager_id 1) (hash 'i_item_sk 2 'i_brand_id 20 'i_manager_id 1) (hash 'i_item_sk 3 'i_brand_id 10 'i_manager_id 1)))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2001 'd_moy 11)))
(define grouped (let ([groups (make-hash)])
  (for* ([ss store_sales] [i item] [d date_dim] #:when (and (and (equal? (hash-ref ss 'item) (hash-ref i 'i_item_sk)) (equal? (hash-ref i 'i_manager_id) 1)) (equal? (hash-ref ss 'sold_date) (hash-ref d 'd_date_sk)))) (let* ([key (hash 'brand_id (hash-ref i 'i_brand_id))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ss ss 'i i 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'brand_id (hash-ref (hash-ref g 'key) 'brand_id) 'ext_price (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v))))))
(define result (let ([_items0 (for*/list ([g grouped]) (hash 'g g))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((g (hash-ref a 'g))) (list (- (hash-ref g 'ext_price)) (hash-ref g 'brand_id)))) (string<? (let ((g (hash-ref a 'g))) (list (- (hash-ref g 'ext_price)) (hash-ref g 'brand_id))) (let ((g (hash-ref b 'g))) (list (- (hash-ref g 'ext_price)) (hash-ref g 'brand_id))))] [(string? (let ((g (hash-ref b 'g))) (list (- (hash-ref g 'ext_price)) (hash-ref g 'brand_id)))) (string<? (let ((g (hash-ref a 'g))) (list (- (hash-ref g 'ext_price)) (hash-ref g 'brand_id))) (let ((g (hash-ref b 'g))) (list (- (hash-ref g 'ext_price)) (hash-ref g 'brand_id))))] [else (< (let ((g (hash-ref a 'g))) (list (- (hash-ref g 'ext_price)) (hash-ref g 'brand_id))) (let ((g (hash-ref b 'g))) (list (- (hash-ref g 'ext_price)) (hash-ref g 'brand_id))))]))))
  (for/list ([item _items0]) (let ((g (hash-ref item 'g))) g))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'brand_id 10 'ext_price 35) (hash 'brand_id 20 'ext_price 20))) (displayln "ok"))
