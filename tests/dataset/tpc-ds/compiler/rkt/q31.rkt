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

(define store_sales (list (hash 'ca_county "A" 'd_qoy 1 'd_year 2000 'ss_ext_sales_price 100) (hash 'ca_county "A" 'd_qoy 2 'd_year 2000 'ss_ext_sales_price 120) (hash 'ca_county "A" 'd_qoy 3 'd_year 2000 'ss_ext_sales_price 160) (hash 'ca_county "B" 'd_qoy 1 'd_year 2000 'ss_ext_sales_price 80) (hash 'ca_county "B" 'd_qoy 2 'd_year 2000 'ss_ext_sales_price 90) (hash 'ca_county "B" 'd_qoy 3 'd_year 2000 'ss_ext_sales_price 100)))
(define web_sales (list (hash 'ca_county "A" 'd_qoy 1 'd_year 2000 'ws_ext_sales_price 100) (hash 'ca_county "A" 'd_qoy 2 'd_year 2000 'ws_ext_sales_price 150) (hash 'ca_county "A" 'd_qoy 3 'd_year 2000 'ws_ext_sales_price 250) (hash 'ca_county "B" 'd_qoy 1 'd_year 2000 'ws_ext_sales_price 80) (hash 'ca_county "B" 'd_qoy 2 'd_year 2000 'ws_ext_sales_price 90) (hash 'ca_county "B" 'd_qoy 3 'd_year 2000 'ws_ext_sales_price 95)))
(define counties '("A" "B"))
(define result '())
(for ([county (if (hash? counties) (hash-keys counties) counties)])
(define ss1 (apply + (for*/list ([v (for*/list ([s store_sales] #:when (and (and (equal? (hash-ref s 'ca_county) county) (equal? (hash-ref s 'd_qoy) 1)))) (hash-ref s 'ss_ext_sales_price))]) v)))
(define ss2 (apply + (for*/list ([v (for*/list ([s store_sales] #:when (and (and (equal? (hash-ref s 'ca_county) county) (equal? (hash-ref s 'd_qoy) 2)))) (hash-ref s 'ss_ext_sales_price))]) v)))
(define ss3 (apply + (for*/list ([v (for*/list ([s store_sales] #:when (and (and (equal? (hash-ref s 'ca_county) county) (equal? (hash-ref s 'd_qoy) 3)))) (hash-ref s 'ss_ext_sales_price))]) v)))
(define ws1 (apply + (for*/list ([v (for*/list ([w web_sales] #:when (and (and (equal? (hash-ref w 'ca_county) county) (equal? (hash-ref w 'd_qoy) 1)))) (hash-ref w 'ws_ext_sales_price))]) v)))
(define ws2 (apply + (for*/list ([v (for*/list ([w web_sales] #:when (and (and (equal? (hash-ref w 'ca_county) county) (equal? (hash-ref w 'd_qoy) 2)))) (hash-ref w 'ws_ext_sales_price))]) v)))
(define ws3 (apply + (for*/list ([v (for*/list ([w web_sales] #:when (and (and (equal? (hash-ref w 'ca_county) county) (equal? (hash-ref w 'd_qoy) 3)))) (hash-ref w 'ws_ext_sales_price))]) v)))
(define web_g1 (/ ws2 ws1))
(define store_g1 (/ ss2 ss1))
(define web_g2 (/ ws3 ws2))
(define store_g2 (/ ss3 ss2))
(if (and (_gt web_g1 store_g1) (_gt web_g2 store_g2))
  (begin
(set! result (append result (list (hash 'ca_county county 'd_year 2000 'web_q1_q2_increase web_g1 'store_q1_q2_increase store_g1 'web_q2_q3_increase web_g2 'store_q2_q3_increase store_g2))))
  )
  (void)
)
)
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'ca_county "A" 'd_year 2000 'web_q1_q2_increase 1.5 'store_q1_q2_increase 1.2 'web_q2_q3_increase 1.6666666666666667 'store_q2_q3_increase 1.3333333333333333))) (displayln "ok"))
