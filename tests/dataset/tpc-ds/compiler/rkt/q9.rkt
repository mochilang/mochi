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

(define store_sales (list (hash 'ss_quantity 5 'ss_ext_discount_amt 5 'ss_net_paid 7) (hash 'ss_quantity 30 'ss_ext_discount_amt 10 'ss_net_paid 15) (hash 'ss_quantity 50 'ss_ext_discount_amt 20 'ss_net_paid 30) (hash 'ss_quantity 70 'ss_ext_discount_amt 25 'ss_net_paid 35) (hash 'ss_quantity 90 'ss_ext_discount_amt 40 'ss_net_paid 50)))
(define reason (list (hash 'r_reason_sk 1)))
(define bucket1 (if (_gt (if (and (hash? (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 1) (_le (hash-ref s 'ss_quantity) 20)))) s)) (hash-has-key? (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 1) (_le (hash-ref s 'ss_quantity) 20)))) s) 'items)) (length (hash-ref (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 1) (_le (hash-ref s 'ss_quantity) 20)))) s) 'items)) (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 1) (_le (hash-ref s 'ss_quantity) 20)))) s))) 10) (let ([xs (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 1) (_le (hash-ref s 'ss_quantity) 20)))) (hash-ref s 'ss_ext_discount_amt))] [n (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 1) (_le (hash-ref s 'ss_quantity) 20)))) (hash-ref s 'ss_ext_discount_amt)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) (let ([xs (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 1) (_le (hash-ref s 'ss_quantity) 20)))) (hash-ref s 'ss_net_paid))] [n (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 1) (_le (hash-ref s 'ss_quantity) 20)))) (hash-ref s 'ss_net_paid)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))
(define bucket2 (if (_gt (if (and (hash? (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 21) (_le (hash-ref s 'ss_quantity) 40)))) s)) (hash-has-key? (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 21) (_le (hash-ref s 'ss_quantity) 40)))) s) 'items)) (length (hash-ref (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 21) (_le (hash-ref s 'ss_quantity) 40)))) s) 'items)) (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 21) (_le (hash-ref s 'ss_quantity) 40)))) s))) 20) (let ([xs (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 21) (_le (hash-ref s 'ss_quantity) 40)))) (hash-ref s 'ss_ext_discount_amt))] [n (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 21) (_le (hash-ref s 'ss_quantity) 40)))) (hash-ref s 'ss_ext_discount_amt)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) (let ([xs (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 21) (_le (hash-ref s 'ss_quantity) 40)))) (hash-ref s 'ss_net_paid))] [n (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 21) (_le (hash-ref s 'ss_quantity) 40)))) (hash-ref s 'ss_net_paid)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))
(define bucket3 (if (_gt (if (and (hash? (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 41) (_le (hash-ref s 'ss_quantity) 60)))) s)) (hash-has-key? (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 41) (_le (hash-ref s 'ss_quantity) 60)))) s) 'items)) (length (hash-ref (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 41) (_le (hash-ref s 'ss_quantity) 60)))) s) 'items)) (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 41) (_le (hash-ref s 'ss_quantity) 60)))) s))) 30) (let ([xs (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 41) (_le (hash-ref s 'ss_quantity) 60)))) (hash-ref s 'ss_ext_discount_amt))] [n (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 41) (_le (hash-ref s 'ss_quantity) 60)))) (hash-ref s 'ss_ext_discount_amt)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) (let ([xs (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 41) (_le (hash-ref s 'ss_quantity) 60)))) (hash-ref s 'ss_net_paid))] [n (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 41) (_le (hash-ref s 'ss_quantity) 60)))) (hash-ref s 'ss_net_paid)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))
(define bucket4 (if (_gt (if (and (hash? (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 61) (_le (hash-ref s 'ss_quantity) 80)))) s)) (hash-has-key? (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 61) (_le (hash-ref s 'ss_quantity) 80)))) s) 'items)) (length (hash-ref (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 61) (_le (hash-ref s 'ss_quantity) 80)))) s) 'items)) (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 61) (_le (hash-ref s 'ss_quantity) 80)))) s))) 40) (let ([xs (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 61) (_le (hash-ref s 'ss_quantity) 80)))) (hash-ref s 'ss_ext_discount_amt))] [n (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 61) (_le (hash-ref s 'ss_quantity) 80)))) (hash-ref s 'ss_ext_discount_amt)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) (let ([xs (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 61) (_le (hash-ref s 'ss_quantity) 80)))) (hash-ref s 'ss_net_paid))] [n (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 61) (_le (hash-ref s 'ss_quantity) 80)))) (hash-ref s 'ss_net_paid)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))
(define bucket5 (if (_gt (if (and (hash? (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 81) (_le (hash-ref s 'ss_quantity) 100)))) s)) (hash-has-key? (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 81) (_le (hash-ref s 'ss_quantity) 100)))) s) 'items)) (length (hash-ref (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 81) (_le (hash-ref s 'ss_quantity) 100)))) s) 'items)) (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 81) (_le (hash-ref s 'ss_quantity) 100)))) s))) 50) (let ([xs (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 81) (_le (hash-ref s 'ss_quantity) 100)))) (hash-ref s 'ss_ext_discount_amt))] [n (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 81) (_le (hash-ref s 'ss_quantity) 100)))) (hash-ref s 'ss_ext_discount_amt)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))) (let ([xs (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 81) (_le (hash-ref s 'ss_quantity) 100)))) (hash-ref s 'ss_net_paid))] [n (length (for*/list ([s store_sales] #:when (and (and (_ge (hash-ref s 'ss_quantity) 81) (_le (hash-ref s 'ss_quantity) 100)))) (hash-ref s 'ss_net_paid)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))
(define result (for*/list ([r reason] #:when (and (equal? (hash-ref r 'r_reason_sk) 1))) (hash 'bucket1 bucket1 'bucket2 bucket2 'bucket3 bucket3 'bucket4 bucket4 'bucket5 bucket5)))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'bucket1 7 'bucket2 15 'bucket3 30 'bucket4 35 'bucket5 50))) (displayln "ok"))
