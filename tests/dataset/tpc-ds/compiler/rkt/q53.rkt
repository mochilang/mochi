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

(define item (list (hash 'i_item_sk 1 'i_manufact_id 1) (hash 'i_item_sk 2 'i_manufact_id 2)))
(define store_sales (list (hash 'item 1 'date 1 'price 10) (hash 'item 1 'date 2 'price 10) (hash 'item 2 'date 1 'price 30) (hash 'item 2 'date 2 'price 23)))
(define date_dim (list (hash 'd_date_sk 1 'd_month_seq 1) (hash 'd_date_sk 2 'd_month_seq 2)))
(define (abs x)
  (let/ec return
(if (_ge x 0)
  (begin
(return x)
  )
  (void)
)
(return (- x))
  ))
(define grouped (let ([groups (make-hash)])
  (for* ([ss store_sales] [i item] [d date_dim] #:when (and (equal? (hash-ref ss 'item) (hash-ref i 'i_item_sk)) (equal? (hash-ref ss 'date) (hash-ref d 'd_date_sk)))) (let* ([key (hash-ref i 'i_manufact_id)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ss ss 'i i 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'manu (hash-ref g 'key) 'sum_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v)) 'avg_sales (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))))
(define result (for*/list ([g grouped] #:when (and (and (_gt (hash-ref g 'avg_sales) 0) (_gt (/ (abs (- (hash-ref g 'sum_sales) (hash-ref g 'avg_sales))) (hash-ref g 'avg_sales)) 0.1)))) (hash 'i_manufact_id (hash-ref g 'manu) 'sum_sales (hash-ref g 'sum_sales))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_manufact_id 1 'sum_sales 20) (hash 'i_manufact_id 2 'sum_sales 53))) (displayln "ok"))
