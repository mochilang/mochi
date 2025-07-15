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

(define store_sales (list (hash 'customer 1 'sold_date 2 'price 60) (hash 'customer 2 'sold_date 2 'price 40)))
(define date_dim (list (hash 'd_date_sk 2 'd_month_seq 5)))
(define customer (list (hash 'c_customer_sk 1 'c_current_addr_sk 1) (hash 'c_customer_sk 2 'c_current_addr_sk 1)))
(define customer_address (list (hash 'ca_address_sk 1 'ca_county "X" 'ca_state "Y")))
(define store (list (hash 's_store_sk 1 's_county "X" 's_state "Y")))
(define (int x)
  (let/ec return
(return (string->number x))
  ))
(define revenue (for*/list ([ss store_sales] [d date_dim] [c customer] [ca customer_address] [s store] #:when (and (equal? (hash-ref ss 'sold_date) (hash-ref d 'd_date_sk)) (equal? (hash-ref ss 'customer) (hash-ref c 'c_customer_sk)) (and (and (equal? (hash-ref c 'c_current_addr_sk) (hash-ref ca 'ca_address_sk)) (string=? (hash-ref ca 'ca_county) "X")) (string=? (hash-ref ca 'ca_state) "Y")) (and (and (equal? 1 (hash-ref s 's_store_sk)) (equal? (hash-ref ca 'ca_county) (hash-ref s 's_county))) (equal? (hash-ref ca 'ca_state) (hash-ref s 's_state))))) (hash 'customer (hash-ref c 'c_customer_sk) 'amt (hash-ref ss 'price))))
(define by_customer (let ([groups (make-hash)])
  (for* ([r revenue]) (let* ([key (hash-ref r 'customer)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons r bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'customer (hash-ref g 'key) 'revenue (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'amt))]) v))))))
(define segments (let ([groups (make-hash)])
  (for* ([r by_customer]) (let* ([key (hash 'seg (int (/ (hash-ref r 'revenue) 50)))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons r bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'segment (hash-ref (hash-ref g 'key) 'seg) 'num_customers (length (hash-ref g 'items)) 'segment_base (* (hash-ref (hash-ref g 'key) 'seg) 50)))))
(define result segments)
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'segment 1 'num_customers 1 'segment_base 50) (hash 'segment 0 'num_customers 1 'segment_base 0))) (displayln "ok"))
