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

(define customer (list (hash 'c_customer_sk 1 'c_last_name "Smith" 'c_first_name "John") (hash 'c_customer_sk 2 'c_last_name "Jones" 'c_first_name "Alice")))
(define store_sales (list (hash 'ss_customer_sk 1 'd_month_seq 1200) (hash 'ss_customer_sk 2 'd_month_seq 1205)))
(define catalog_sales (list (hash 'cs_bill_customer_sk 1 'd_month_seq 1203)))
(define web_sales (list (hash 'ws_bill_customer_sk 1 'd_month_seq 1206)))
(define (distinct xs)
  (let/ec return
(define out '())
(for ([x (if (hash? xs) (hash-keys xs) xs)])
(if (not (contains out x))
  (begin
(set! out (append out (list x)))
  )
  (void)
)
)
(return out)
  ))
(define store_ids (distinct (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'd_month_seq) 1200) (_le (hash-ref s 'd_month_seq) 1211)))) (hash-ref s 'ss_customer_sk))))
(define catalog_ids (distinct (for*/list ([c catalog_sales] #:when (and (and (_ge (hash-ref c 'd_month_seq) 1200) (_le (hash-ref c 'd_month_seq) 1211)))) (hash-ref c 'cs_bill_customer_sk))))
(define web_ids (distinct (for*/list ([w web_sales] #:when (and (and (_ge (hash-ref w 'd_month_seq) 1200) (_le (hash-ref w 'd_month_seq) 1211)))) (hash-ref w 'ws_bill_customer_sk))))
(define hot (filter (lambda (x) (member x web_ids)) (filter (lambda (x) (member x catalog_ids)) store_ids)))
(define result (cond [(string? hot) (string-length hot)] [(hash? hot) (hash-count hot)] [else (length hot)]))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result 1) (displayln "ok"))
