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

(struct StoreSale (ss_quantity ss_list_price ss_coupon_amt ss_wholesale_cost) #:transparent #:mutable)
(define store_sales (list (hash 'ss_quantity 3 'ss_list_price 100 'ss_coupon_amt 50 'ss_wholesale_cost 30) (hash 'ss_quantity 8 'ss_list_price 80 'ss_coupon_amt 10 'ss_wholesale_cost 20) (hash 'ss_quantity 12 'ss_list_price 60 'ss_coupon_amt 5 'ss_wholesale_cost 15)))
(define bucket1 (for*/list ([ss store_sales] #:when (and (and (and (_ge (hash-ref ss 'ss_quantity) 0) (_le (hash-ref ss 'ss_quantity) 5)) (or (or (and (_ge (hash-ref ss 'ss_list_price) 0) (_le (hash-ref ss 'ss_list_price) 110)) (and (_ge (hash-ref ss 'ss_coupon_amt) 0) (_le (hash-ref ss 'ss_coupon_amt) 1000))) (and (_ge (hash-ref ss 'ss_wholesale_cost) 0) (_le (hash-ref ss 'ss_wholesale_cost) 50)))))) ss))
(define bucket2 (for*/list ([ss store_sales] #:when (and (and (and (_ge (hash-ref ss 'ss_quantity) 6) (_le (hash-ref ss 'ss_quantity) 10)) (or (or (and (_ge (hash-ref ss 'ss_list_price) 0) (_le (hash-ref ss 'ss_list_price) 110)) (and (_ge (hash-ref ss 'ss_coupon_amt) 0) (_le (hash-ref ss 'ss_coupon_amt) 1000))) (and (_ge (hash-ref ss 'ss_wholesale_cost) 0) (_le (hash-ref ss 'ss_wholesale_cost) 50)))))) ss))
(define result (hash 'B1_LP (let ([xs (for*/list ([x bucket1]) (hash-ref x 'ss_list_price))] [n (length (for*/list ([x bucket1]) (hash-ref x 'ss_list_price)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'B1_CNT (if (and (hash? bucket1) (hash-has-key? bucket1 'items)) (length (hash-ref bucket1 'items)) (length bucket1)) 'B1_CNTD (if (and (hash? (let ([groups (make-hash)])
  (for* ([x bucket1]) (let* ([key (hash-ref x 'ss_list_price)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons x bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash-ref g 'key)))) (hash-has-key? (let ([groups (make-hash)])
  (for* ([x bucket1]) (let* ([key (hash-ref x 'ss_list_price)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons x bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash-ref g 'key))) 'items)) (length (hash-ref (let ([groups (make-hash)])
  (for* ([x bucket1]) (let* ([key (hash-ref x 'ss_list_price)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons x bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash-ref g 'key))) 'items)) (length (let ([groups (make-hash)])
  (for* ([x bucket1]) (let* ([key (hash-ref x 'ss_list_price)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons x bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash-ref g 'key))))) 'B2_LP (let ([xs (for*/list ([x bucket2]) (hash-ref x 'ss_list_price))] [n (length (for*/list ([x bucket2]) (hash-ref x 'ss_list_price)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) 'B2_CNT (if (and (hash? bucket2) (hash-has-key? bucket2 'items)) (length (hash-ref bucket2 'items)) (length bucket2)) 'B2_CNTD (if (and (hash? (let ([groups (make-hash)])
  (for* ([x bucket2]) (let* ([key (hash-ref x 'ss_list_price)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons x bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash-ref g 'key)))) (hash-has-key? (let ([groups (make-hash)])
  (for* ([x bucket2]) (let* ([key (hash-ref x 'ss_list_price)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons x bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash-ref g 'key))) 'items)) (length (hash-ref (let ([groups (make-hash)])
  (for* ([x bucket2]) (let* ([key (hash-ref x 'ss_list_price)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons x bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash-ref g 'key))) 'items)) (length (let ([groups (make-hash)])
  (for* ([x bucket2]) (let* ([key (hash-ref x 'ss_list_price)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons x bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash-ref g 'key)))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (hash 'B1_LP 100 'B1_CNT 1 'B1_CNTD 1 'B2_LP 80 'B2_CNT 1 'B2_CNTD 1)) (displayln "ok"))
