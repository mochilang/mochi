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

(define nation (list (hash 'n_nationkey 1 'n_name "CANADA") (hash 'n_nationkey 2 'n_name "GERMANY")))
(define supplier (list (hash 's_suppkey 100 's_name "Maple Supply" 's_address "123 Forest Lane" 's_nationkey 1) (hash 's_suppkey 200 's_name "Berlin Metals" 's_address "456 Iron Str" 's_nationkey 2)))
(define part (list (hash 'p_partkey 10 'p_name "forest glade bricks") (hash 'p_partkey 20 'p_name "desert sand paper")))
(define partsupp (list (hash 'ps_partkey 10 'ps_suppkey 100 'ps_availqty 100) (hash 'ps_partkey 20 'ps_suppkey 200 'ps_availqty 30)))
(define lineitem (list (hash 'l_partkey 10 'l_suppkey 100 'l_quantity 100 'l_shipdate "1994-05-15") (hash 'l_partkey 10 'l_suppkey 100 'l_quantity 50 'l_shipdate "1995-01-01")))
(define prefix "forest")
(define shipped_94 (let ([groups (make-hash)])
  (for* ([l lineitem] #:when (and (and (string>=? (hash-ref l 'l_shipdate) "1994-01-01") (string<? (hash-ref l 'l_shipdate) "1995-01-01")))) (let* ([key (hash 'partkey (hash-ref l 'l_partkey) 'suppkey (hash-ref l 'l_suppkey))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons l bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'partkey (hash-ref (hash-ref g 'key) 'partkey) 'suppkey (hash-ref (hash-ref g 'key) 'suppkey) 'qty (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'l_quantity))]) v))))))
(define target_partkeys (for*/list ([ps partsupp] [p part] [s shipped_94] #:when (and (equal? (hash-ref ps 'ps_partkey) (hash-ref p 'p_partkey)) (and (equal? (hash-ref ps 'ps_partkey) (hash-ref s 'partkey)) (equal? (hash-ref ps 'ps_suppkey) (hash-ref s 'suppkey))) (and (string=? (substring (hash-ref p 'p_name) 0 (string-length prefix)) prefix) (_gt (hash-ref ps 'ps_availqty) (* 0.5 (hash-ref s 'qty)))))) (hash-ref ps 'ps_suppkey)))
(define result (let ([_items0 (for*/list ([s supplier] [n nation] #:when (and (equal? (hash-ref n 'n_nationkey) (hash-ref s 's_nationkey)) (and (cond [(string? target_partkeys) (regexp-match? (regexp (hash-ref s 's_suppkey)) target_partkeys)] [(hash? target_partkeys) (hash-has-key? target_partkeys (hash-ref s 's_suppkey))] [else (member (hash-ref s 's_suppkey) target_partkeys)]) (string=? (hash-ref n 'n_name) "CANADA")))) (hash 's s 'n n))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((s (hash-ref a 's)) (n (hash-ref a 'n))) (hash-ref s 's_name))) (string<? (let ((s (hash-ref a 's)) (n (hash-ref a 'n))) (hash-ref s 's_name)) (let ((s (hash-ref b 's)) (n (hash-ref b 'n))) (hash-ref s 's_name)))] [(string? (let ((s (hash-ref b 's)) (n (hash-ref b 'n))) (hash-ref s 's_name))) (string<? (let ((s (hash-ref a 's)) (n (hash-ref a 'n))) (hash-ref s 's_name)) (let ((s (hash-ref b 's)) (n (hash-ref b 'n))) (hash-ref s 's_name)))] [else (< (let ((s (hash-ref a 's)) (n (hash-ref a 'n))) (hash-ref s 's_name)) (let ((s (hash-ref b 's)) (n (hash-ref b 'n))) (hash-ref s 's_name)))]))))
  (for/list ([item _items0]) (let ((s (hash-ref item 's)) (n (hash-ref item 'n))) (hash 's_name (hash-ref s 's_name) 's_address (hash-ref s 's_address))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 's_name "Maple Supply" 's_address "123 Forest Lane"))) (displayln "ok"))
