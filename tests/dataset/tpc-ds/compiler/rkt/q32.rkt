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

(define catalog_sales (list (hash 'cs_item_sk 1 'cs_sold_date_sk 1 'cs_ext_discount_amt 5) (hash 'cs_item_sk 1 'cs_sold_date_sk 2 'cs_ext_discount_amt 10) (hash 'cs_item_sk 1 'cs_sold_date_sk 3 'cs_ext_discount_amt 20)))
(define item (list (hash 'i_item_sk 1 'i_manufact_id 1)))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2000) (hash 'd_date_sk 2 'd_year 2000) (hash 'd_date_sk 3 'd_year 2000)))
(define filtered (for*/list ([cs catalog_sales] [i item] [d date_dim] #:when (and (equal? (hash-ref cs 'cs_item_sk) (hash-ref i 'i_item_sk)) (equal? (hash-ref cs 'cs_sold_date_sk) (hash-ref d 'd_date_sk)) (and (equal? (hash-ref i 'i_manufact_id) 1) (equal? (hash-ref d 'd_year) 2000)))) (hash-ref cs 'cs_ext_discount_amt)))
(define avg_discount (let ([xs filtered] [n (length filtered)]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))))
(define result (apply + (for*/list ([v (for*/list ([x filtered] #:when (and (_gt x (* avg_discount 1.3)))) x)]) v)))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result 20) (displayln "ok"))
