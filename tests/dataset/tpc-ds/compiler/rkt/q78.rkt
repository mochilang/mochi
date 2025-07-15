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

(define ss (list (hash 'ss_sold_year 1998 'ss_item_sk 1 'ss_customer_sk 1 'ss_qty 10 'ss_wc 50 'ss_sp 100)))
(define ws (list (hash 'ws_sold_year 1998 'ws_item_sk 1 'ws_customer_sk 1 'ws_qty 5 'ws_wc 25 'ws_sp 50)))
(define cs (list (hash 'cs_sold_year 1998 'cs_item_sk 1 'cs_customer_sk 1 'cs_qty 3 'cs_wc 15 'cs_sp 30)))
(define result (for*/list ([s ss] #:when (and (and (or (_gt (if (equal? w '()) 0 (hash-ref w 'ws_qty)) 0) (_gt (if (equal? c '()) 0 (hash-ref c 'cs_qty)) 0)) (equal? (hash-ref s 'ss_sold_year) 1998)))) (let ((w (findf (lambda (w) (and (and (equal? (hash-ref w 'ws_sold_year) (hash-ref s 'ss_sold_year)) (equal? (hash-ref w 'ws_item_sk) (hash-ref s 'ss_item_sk))) (equal? (hash-ref w 'ws_customer_sk) (hash-ref s 'ss_customer_sk)))) ws)) (c (findf (lambda (c) (and (and (equal? (hash-ref c 'cs_sold_year) (hash-ref s 'ss_sold_year)) (equal? (hash-ref c 'cs_item_sk) (hash-ref s 'ss_item_sk))) (equal? (hash-ref c 'cs_customer_sk) (hash-ref s 'ss_customer_sk)))) cs))) (hash 'ss_sold_year (hash-ref s 'ss_sold_year) 'ss_item_sk (hash-ref s 'ss_item_sk) 'ss_customer_sk (hash-ref s 'ss_customer_sk) 'ratio (/ (hash-ref s 'ss_qty) (+ (if (equal? w '()) 0 (hash-ref w 'ws_qty)) (if (equal? c '()) 0 (hash-ref c 'cs_qty)))) 'store_qty (hash-ref s 'ss_qty) 'store_wholesale_cost (hash-ref s 'ss_wc) 'store_sales_price (hash-ref s 'ss_sp) 'other_chan_qty (+ (if (equal? w '()) 0 (hash-ref w 'ws_qty)) (if (equal? c '()) 0 (hash-ref c 'cs_qty))) 'other_chan_wholesale_cost (+ (if (equal? w '()) 0 (hash-ref w 'ws_wc)) (if (equal? c '()) 0 (hash-ref c 'cs_wc))) 'other_chan_sales_price (+ (if (equal? w '()) 0 (hash-ref w 'ws_sp)) (if (equal? c '()) 0 (hash-ref c 'cs_sp)))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'ss_sold_year 1998 'ss_item_sk 1 'ss_customer_sk 1 'ratio 1.25 'store_qty 10 'store_wholesale_cost 50 'store_sales_price 100 'other_chan_qty 8 'other_chan_wholesale_cost 40 'other_chan_sales_price 80))) (displayln "ok"))
