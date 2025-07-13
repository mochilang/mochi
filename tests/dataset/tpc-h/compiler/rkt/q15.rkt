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

(define supplier (list (hash 's_suppkey 100 's_name "Best Supplier" 's_address "123 Market St" 's_phone "123-456") (hash 's_suppkey 200 's_name "Second Supplier" 's_address "456 Elm St" 's_phone "987-654")))
(define lineitem (list (hash 'l_suppkey 100 'l_extendedprice 1000 'l_discount 0.1 'l_shipdate "1996-01-15") (hash 'l_suppkey 100 'l_extendedprice 500 'l_discount 0 'l_shipdate "1996-03-20") (hash 'l_suppkey 200 'l_extendedprice 800 'l_discount 0.05 'l_shipdate "1995-12-30")))
(define start_date "1996-01-01")
(define end_date "1996-04-01")
(define revenue0 (let ([groups (make-hash)])
  (for* ([l lineitem] #:when (and (and (string>=? (hash-ref l 'l_shipdate) start_date) (string<? (hash-ref l 'l_shipdate) end_date)))) (let* ([key (hash-ref l 'l_suppkey)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons l bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'supplier_no (hash-ref g 'key) 'total_revenue (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (* (hash-ref x 'l_extendedprice) (- 1 (hash-ref x 'l_discount))))]) v))))))
(define revenues (for*/list ([x revenue0]) (hash-ref x 'total_revenue)))
(define max_revenue (_max revenues))
(define result (let ([_items0 (for*/list ([s supplier] [r revenue0] #:when (and (equal? (hash-ref s 's_suppkey) (hash-ref r 'supplier_no)) (equal? (hash-ref r 'total_revenue) max_revenue))) (hash 's s 'r r))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((s (hash-ref a 's)) (r (hash-ref a 'r))) (hash-ref s 's_suppkey))) (string<? (let ((s (hash-ref a 's)) (r (hash-ref a 'r))) (hash-ref s 's_suppkey)) (let ((s (hash-ref b 's)) (r (hash-ref b 'r))) (hash-ref s 's_suppkey)))] [(string? (let ((s (hash-ref b 's)) (r (hash-ref b 'r))) (hash-ref s 's_suppkey))) (string<? (let ((s (hash-ref a 's)) (r (hash-ref a 'r))) (hash-ref s 's_suppkey)) (let ((s (hash-ref b 's)) (r (hash-ref b 'r))) (hash-ref s 's_suppkey)))] [else (< (let ((s (hash-ref a 's)) (r (hash-ref a 'r))) (hash-ref s 's_suppkey)) (let ((s (hash-ref b 's)) (r (hash-ref b 'r))) (hash-ref s 's_suppkey)))]))))
  (for/list ([item _items0]) (let ((s (hash-ref item 's)) (r (hash-ref item 'r))) (hash 's_suppkey (hash-ref s 's_suppkey) 's_name (hash-ref s 's_name) 's_address (hash-ref s 's_address) 's_phone (hash-ref s 's_phone) 'total_revenue (hash-ref r 'total_revenue))))))
(displayln (jsexpr->string result))
(define rev (+ (* 1000 0.9) 500))
(when (equal? result (list (hash 's_suppkey 100 's_name "Best Supplier" 's_address "123 Market St" 's_phone "123-456" 'total_revenue rev))) (displayln "ok"))
