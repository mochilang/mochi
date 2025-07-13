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

(define nation (list (hash 'n_nationkey 1 'n_name "GERMANY") (hash 'n_nationkey 2 'n_name "FRANCE")))
(define supplier (list (hash 's_suppkey 100 's_nationkey 1) (hash 's_suppkey 200 's_nationkey 1) (hash 's_suppkey 300 's_nationkey 2)))
(define partsupp (list (hash 'ps_partkey 1000 'ps_suppkey 100 'ps_supplycost 10 'ps_availqty 100) (hash 'ps_partkey 1000 'ps_suppkey 200 'ps_supplycost 20 'ps_availqty 50) (hash 'ps_partkey 2000 'ps_suppkey 100 'ps_supplycost 5 'ps_availqty 10) (hash 'ps_partkey 3000 'ps_suppkey 300 'ps_supplycost 8 'ps_availqty 500)))
(define target_nation "GERMANY")
(define filtered (for*/list ([ps partsupp] [s supplier] [n nation] #:when (and (equal? (hash-ref s 's_suppkey) (hash-ref ps 'ps_suppkey)) (equal? (hash-ref n 'n_nationkey) (hash-ref s 's_nationkey)) (string=? (hash-ref n 'n_name) target_nation))) (hash 'ps_partkey (hash-ref ps 'ps_partkey) 'value (* (hash-ref ps 'ps_supplycost) (hash-ref ps 'ps_availqty)))))
(define grouped (let ([groups (make-hash)])
  (for* ([x filtered]) (let* ([key (hash-ref x 'ps_partkey)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons x bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'ps_partkey (hash-ref g 'key) 'value (apply + (for*/list ([v (for*/list ([r (hash-ref g 'items)]) (hash-ref r 'value))]) v))))))
(define total (apply + (for*/list ([v (for*/list ([x filtered]) (hash-ref x 'value))]) v)))
(define threshold (* total 0.0001))
(define result (let ([_items0 (for*/list ([x grouped] #:when (and (_gt (hash-ref x 'value) threshold))) (hash 'x x))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((x (hash-ref a 'x))) (hash-ref x 'value))) (string>? (let ((x (hash-ref a 'x))) (hash-ref x 'value)) (let ((x (hash-ref b 'x))) (hash-ref x 'value)))] [(string? (let ((x (hash-ref b 'x))) (hash-ref x 'value))) (string>? (let ((x (hash-ref a 'x))) (hash-ref x 'value)) (let ((x (hash-ref b 'x))) (hash-ref x 'value)))] [else (> (let ((x (hash-ref a 'x))) (hash-ref x 'value)) (let ((x (hash-ref b 'x))) (hash-ref x 'value)))]))))
  (for/list ([item _items0]) (let ((x (hash-ref item 'x))) x))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'ps_partkey 1000 'value 2000) (hash 'ps_partkey 2000 'value 50))) (displayln "ok"))
