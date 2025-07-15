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

(struct WebSale (ws_item_sk ws_sold_date_sk ws_ext_discount_amt) #:transparent #:mutable)
(define web_sales (list (hash 'ws_item_sk 1 'ws_sold_date_sk 1 'ws_ext_discount_amt 1) (hash 'ws_item_sk 1 'ws_sold_date_sk 1 'ws_ext_discount_amt 1) (hash 'ws_item_sk 1 'ws_sold_date_sk 1 'ws_ext_discount_amt 2)))
(define item (list (hash 'i_item_sk 1 'i_manufact_id 1)))
(define date_dim (list (hash 'd_date_sk 1 'd_date "2000-01-02")))
(define sum_amt (apply + (for*/list ([v (for*/list ([ws web_sales]) (hash-ref ws 'ws_ext_discount_amt))]) v)))
(define avg_amt (let ([xs (for*/list ([ws web_sales]) (hash-ref ws 'ws_ext_discount_amt))] [n (length (for*/list ([ws web_sales]) (hash-ref ws 'ws_ext_discount_amt)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))))
(define result (if (_gt sum_amt (* avg_amt 1.3)) sum_amt 0))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result 4) (displayln "ok"))
