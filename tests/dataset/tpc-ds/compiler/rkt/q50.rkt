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

(define store_sales (list (hash 'ticket 1 'item 101 'sold 1 'customer 1 'store 1) (hash 'ticket 2 'item 102 'sold 1 'customer 1 'store 1) (hash 'ticket 3 'item 103 'sold 1 'customer 1 'store 1) (hash 'ticket 4 'item 104 'sold 1 'customer 1 'store 1) (hash 'ticket 5 'item 105 'sold 1 'customer 1 'store 1)))
(define store_returns (list (hash 'ticket 1 'item 101 'returned 16 'customer 1) (hash 'ticket 2 'item 102 'returned 46 'customer 1) (hash 'ticket 3 'item 103 'returned 76 'customer 1) (hash 'ticket 4 'item 104 'returned 111 'customer 1) (hash 'ticket 5 'item 105 'returned 151 'customer 1)))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2001 'd_moy 7) (hash 'd_date_sk 16 'd_year 2001 'd_moy 8) (hash 'd_date_sk 46 'd_year 2001 'd_moy 8) (hash 'd_date_sk 76 'd_year 2001 'd_moy 8) (hash 'd_date_sk 111 'd_year 2001 'd_moy 8) (hash 'd_date_sk 151 'd_year 2001 'd_moy 8)))
(define store (list (hash 's_store_sk 1 's_store_name "Main" 's_company_id 1 's_street_number "1" 's_street_name "Main" 's_street_type "St" 's_suite_number "100" 's_city "City" 's_county "County" 's_state "CA" 's_zip "12345")))
(define year 2001)
(define month 8)
(define joined (for*/list ([ss store_sales] [sr store_returns] [d1 date_dim] [d2 date_dim] [s store] #:when (and (and (and (equal? (hash-ref ss 'ticket) (hash-ref sr 'ticket)) (equal? (hash-ref ss 'item) (hash-ref sr 'item))) (equal? (hash-ref ss 'customer) (hash-ref sr 'customer))) (equal? (hash-ref ss 'sold) (hash-ref d1 'd_date_sk)) (and (and (equal? (hash-ref sr 'returned) (hash-ref d2 'd_date_sk)) (equal? (hash-ref d2 'd_year) year)) (equal? (hash-ref d2 'd_moy) month)) (equal? (hash-ref ss 'store) (hash-ref s 's_store_sk)))) (hash 's s 'diff (- (hash-ref sr 'returned) (hash-ref ss 'sold)))))
(define result (let ([groups (make-hash)])
  (for* ([j joined]) (let* ([key (hash-ref j 's)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons j bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 's_store_name (hash-ref (hash-ref g 'key) 's_store_name) 'd30 (if (and (hash? (for*/list ([x (hash-ref g 'items)] #:when (and (_le (hash-ref x 'diff) 30))) 1)) (hash-has-key? (for*/list ([x (hash-ref g 'items)] #:when (and (_le (hash-ref x 'diff) 30))) 1) 'items)) (length (hash-ref (for*/list ([x (hash-ref g 'items)] #:when (and (_le (hash-ref x 'diff) 30))) 1) 'items)) (length (for*/list ([x (hash-ref g 'items)] #:when (and (_le (hash-ref x 'diff) 30))) 1))) 'd31_60 (if (and (hash? (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (hash-ref x 'diff) 30) (_le (hash-ref x 'diff) 60)))) 1)) (hash-has-key? (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (hash-ref x 'diff) 30) (_le (hash-ref x 'diff) 60)))) 1) 'items)) (length (hash-ref (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (hash-ref x 'diff) 30) (_le (hash-ref x 'diff) 60)))) 1) 'items)) (length (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (hash-ref x 'diff) 30) (_le (hash-ref x 'diff) 60)))) 1))) 'd61_90 (if (and (hash? (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (hash-ref x 'diff) 60) (_le (hash-ref x 'diff) 90)))) 1)) (hash-has-key? (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (hash-ref x 'diff) 60) (_le (hash-ref x 'diff) 90)))) 1) 'items)) (length (hash-ref (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (hash-ref x 'diff) 60) (_le (hash-ref x 'diff) 90)))) 1) 'items)) (length (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (hash-ref x 'diff) 60) (_le (hash-ref x 'diff) 90)))) 1))) 'd91_120 (if (and (hash? (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (hash-ref x 'diff) 90) (_le (hash-ref x 'diff) 120)))) 1)) (hash-has-key? (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (hash-ref x 'diff) 90) (_le (hash-ref x 'diff) 120)))) 1) 'items)) (length (hash-ref (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (hash-ref x 'diff) 90) (_le (hash-ref x 'diff) 120)))) 1) 'items)) (length (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (hash-ref x 'diff) 90) (_le (hash-ref x 'diff) 120)))) 1))) 'd_gt_120 (if (and (hash? (for*/list ([x (hash-ref g 'items)] #:when (and (_gt (hash-ref x 'diff) 120))) 1)) (hash-has-key? (for*/list ([x (hash-ref g 'items)] #:when (and (_gt (hash-ref x 'diff) 120))) 1) 'items)) (length (hash-ref (for*/list ([x (hash-ref g 'items)] #:when (and (_gt (hash-ref x 'diff) 120))) 1) 'items)) (length (for*/list ([x (hash-ref g 'items)] #:when (and (_gt (hash-ref x 'diff) 120))) 1)))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 's_store_name "Main" 'd30 1 'd31_60 1 'd61_90 1 'd91_120 1 'd_gt_120 1))) (displayln "ok"))
