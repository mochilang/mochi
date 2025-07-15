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

(define catalog_sales (list (hash 'order 1 'item_sk 1 'warehouse_sk 1 'date_sk 1 'price 100) (hash 'order 2 'item_sk 1 'warehouse_sk 1 'date_sk 2 'price 150)))
(define catalog_returns (list (hash 'order 2 'item_sk 1 'refunded 150)))
(define item (list (hash 'item_sk 1 'item_id "I1" 'current_price 1.2)))
(define warehouse (list (hash 'warehouse_sk 1 'state "CA")))
(define date_dim (list (hash 'date_sk 1 'date "2020-01-10") (hash 'date_sk 2 'date "2020-01-20")))
(define sales_date "2020-01-15")
(define records (for*/list ([cs catalog_sales] [w warehouse] [i item] [d date_dim] #:when (and (equal? (hash-ref cs 'warehouse_sk) (hash-ref w 'warehouse_sk)) (equal? (hash-ref cs 'item_sk) (hash-ref i 'item_sk)) (equal? (hash-ref cs 'date_sk) (hash-ref d 'date_sk)) (and (_ge (hash-ref i 'current_price) 0.99) (_le (hash-ref i 'current_price) 1.49)))) (let ((cr (findf (lambda (cr) (and (equal? (hash-ref cs 'order) (hash-ref cr 'order)) (equal? (hash-ref cs 'item_sk) (hash-ref cr 'item_sk)))) catalog_returns))) (hash 'w_state (hash-ref w 'state) 'i_item_id (hash-ref i 'item_id) 'sold_date (hash-ref d 'date) 'net (- (hash-ref cs 'price) (if (equal? cr '()) 0 (hash-ref cr 'refunded)))))))
(define result (let ([groups (make-hash)])
  (for* ([r records]) (let* ([key (hash 'w_state (hash-ref r 'w_state) 'i_item_id (hash-ref r 'i_item_id))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons r bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'w_state (hash-ref (hash-ref g 'key) 'w_state) 'i_item_id (hash-ref (hash-ref g 'key) 'i_item_id) 'sales_before (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (if (string<? (hash-ref x 'sold_date) sales_date) (hash-ref x 'net) 0))]) v)) 'sales_after (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (if (string>=? (hash-ref x 'sold_date) sales_date) (hash-ref x 'net) 0))]) v))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'w_state "CA" 'i_item_id "I1" 'sales_before 100 'sales_after 0))) (displayln "ok"))
