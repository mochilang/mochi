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

(define store_sales (list (hash 'cust "A" 'price 5) (hash 'cust "B" 'price 30) (hash 'cust "C" 'price 57)))
(define catalog_sales (list (hash 'cust "A")))
(define web_sales '())
(define store_customers (for*/list ([s store_sales]) (hash-ref s 'cust)))
(define catalog_customers (for*/list ([s catalog_sales]) (hash-ref s 'cust)))
(define web_customers (for*/list ([s web_sales]) (hash-ref s 'cust)))
(define store_only (for*/list ([c store_customers] #:when (and (and (equal? (cond [(string? (for*/list ([x catalog_customers] #:when (and (equal? x c))) x)) (string-length (for*/list ([x catalog_customers] #:when (and (equal? x c))) x))] [(hash? (for*/list ([x catalog_customers] #:when (and (equal? x c))) x)) (hash-count (for*/list ([x catalog_customers] #:when (and (equal? x c))) x))] [else (length (for*/list ([x catalog_customers] #:when (and (equal? x c))) x))]) 0) (equal? (cond [(string? (for*/list ([x web_customers] #:when (and (equal? x c))) x)) (string-length (for*/list ([x web_customers] #:when (and (equal? x c))) x))] [(hash? (for*/list ([x web_customers] #:when (and (equal? x c))) x)) (hash-count (for*/list ([x web_customers] #:when (and (equal? x c))) x))] [else (length (for*/list ([x web_customers] #:when (and (equal? x c))) x))]) 0)))) c))
(define result (apply + (for*/list ([v (for*/list ([s store_sales] #:when (and (_gt (cond [(string? (for*/list ([x store_only] #:when (and (equal? x (hash-ref s 'cust)))) x)) (string-length (for*/list ([x store_only] #:when (and (equal? x (hash-ref s 'cust)))) x))] [(hash? (for*/list ([x store_only] #:when (and (equal? x (hash-ref s 'cust)))) x)) (hash-count (for*/list ([x store_only] #:when (and (equal? x (hash-ref s 'cust)))) x))] [else (length (for*/list ([x store_only] #:when (and (equal? x (hash-ref s 'cust)))) x))]) 0))) (hash-ref s 'price))]) v)))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result 87) (displayln "ok"))
