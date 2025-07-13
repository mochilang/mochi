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

(define supplier (list (hash 's_suppkey 100 's_name "AlphaSupply" 's_address "123 Hilltop" 's_comment "Reliable and efficient") (hash 's_suppkey 200 's_name "BetaSupply" 's_address "456 Riverside" 's_comment "Known for Customer Complaints")))
(define part (list (hash 'p_partkey 1 'p_brand "Brand#12" 'p_type "SMALL ANODIZED" 'p_size 5) (hash 'p_partkey 2 'p_brand "Brand#23" 'p_type "MEDIUM POLISHED" 'p_size 10)))
(define partsupp (list (hash 'ps_partkey 1 'ps_suppkey 100) (hash 'ps_partkey 2 'ps_suppkey 200)))
(define excluded_suppliers (for*/list ([ps partsupp] [p part] #:when (and (equal? (hash-ref p 'p_partkey) (hash-ref ps 'ps_partkey)) (and (and (string=? (hash-ref p 'p_brand) "Brand#12") (regexp-match? (regexp (regexp-quote "SMALL")) (hash-ref p 'p_type))) (equal? (hash-ref p 'p_size) 5)))) (hash-ref ps 'ps_suppkey)))
(define result (let ([_items0 (for*/list ([s supplier] #:when (and (and (and (not (cond [(string? excluded_suppliers) (regexp-match? (regexp (hash-ref s 's_suppkey)) excluded_suppliers)] [(hash? excluded_suppliers) (hash-has-key? excluded_suppliers (hash-ref s 's_suppkey))] [else (member (hash-ref s 's_suppkey) excluded_suppliers)])) (not (regexp-match? (regexp (regexp-quote "Customer")) (hash-ref s 's_comment)))) (not (regexp-match? (regexp (regexp-quote "Complaints")) (hash-ref s 's_comment)))))) (hash 's s))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((s (hash-ref a 's))) (hash-ref s 's_name))) (string<? (let ((s (hash-ref a 's))) (hash-ref s 's_name)) (let ((s (hash-ref b 's))) (hash-ref s 's_name)))] [(string? (let ((s (hash-ref b 's))) (hash-ref s 's_name))) (string<? (let ((s (hash-ref a 's))) (hash-ref s 's_name)) (let ((s (hash-ref b 's))) (hash-ref s 's_name)))] [else (< (let ((s (hash-ref a 's))) (hash-ref s 's_name)) (let ((s (hash-ref b 's))) (hash-ref s 's_name)))]))))
  (for/list ([item _items0]) (let ((s (hash-ref item 's))) (hash 's_name (hash-ref s 's_name) 's_address (hash-ref s 's_address))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result '()) (displayln "ok"))
