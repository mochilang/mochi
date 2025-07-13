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

(define part (list (hash 'p_partkey 1 'p_brand "Brand#23" 'p_container "MED BOX") (hash 'p_partkey 2 'p_brand "Brand#77" 'p_container "LG JAR")))
(define lineitem (list (hash 'l_partkey 1 'l_quantity 1 'l_extendedprice 100) (hash 'l_partkey 1 'l_quantity 10 'l_extendedprice 1000) (hash 'l_partkey 1 'l_quantity 20 'l_extendedprice 2000) (hash 'l_partkey 2 'l_quantity 5 'l_extendedprice 500)))
(define brand "Brand#23")
(define container "MED BOX")
(define filtered (for*/list ([l lineitem] [p part] #:when (and (equal? (hash-ref p 'p_partkey) (hash-ref l 'l_partkey)) (and (and (string=? (hash-ref p 'p_brand) brand) (string=? (hash-ref p 'p_container) container)) (_lt (hash-ref l 'l_quantity) (* 0.2 (let ([xs (for*/list ([x lineitem] #:when (and (equal? (hash-ref x 'l_partkey) (hash-ref p 'p_partkey)))) (hash-ref x 'l_quantity))] [n (length (for*/list ([x lineitem] #:when (and (equal? (hash-ref x 'l_partkey) (hash-ref p 'p_partkey)))) (hash-ref x 'l_quantity)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))))) (hash-ref l 'l_extendedprice)))
(define result (/ (apply + (for*/list ([v filtered]) v)) 7))
(displayln (jsexpr->string (_json-fix result)))
(define expected (/ 100 7))
(when (equal? result expected) (displayln "ok"))
