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

(define nation (list (hash 'n_nationkey 1 'n_name "SAUDI ARABIA") (hash 'n_nationkey 2 'n_name "FRANCE")))
(define supplier (list (hash 's_suppkey 100 's_name "Desert Trade" 's_nationkey 1) (hash 's_suppkey 200 's_name "Euro Goods" 's_nationkey 2)))
(define orders (list (hash 'o_orderkey 500 'o_orderstatus "F") (hash 'o_orderkey 600 'o_orderstatus "O")))
(define lineitem (list (hash 'l_orderkey 500 'l_suppkey 100 'l_receiptdate "1995-04-15" 'l_commitdate "1995-04-10") (hash 'l_orderkey 500 'l_suppkey 200 'l_receiptdate "1995-04-12" 'l_commitdate "1995-04-12") (hash 'l_orderkey 600 'l_suppkey 100 'l_receiptdate "1995-05-01" 'l_commitdate "1995-04-25")))
(define result (let ([groups (make-hash)])
  (for* ([s supplier] [l1 lineitem] [o orders] [n nation] #:when (and (equal? (hash-ref s 's_suppkey) (hash-ref l1 'l_suppkey)) (equal? (hash-ref o 'o_orderkey) (hash-ref l1 'l_orderkey)) (equal? (hash-ref n 'n_nationkey) (hash-ref s 's_nationkey)) (and (and (and (string=? (hash-ref o 'o_orderstatus) "F") (_gt (hash-ref l1 'l_receiptdate) (hash-ref l1 'l_commitdate))) (string=? (hash-ref n 'n_name) "SAUDI ARABIA")) (not (not (null? (for*/list ([x lineitem] #:when (and (and (and (equal? (hash-ref x 'l_orderkey) (hash-ref l1 'l_orderkey)) (not (equal? (hash-ref x 'l_suppkey) (hash-ref l1 'l_suppkey)))) (_gt (hash-ref x 'l_receiptdate) (hash-ref x 'l_commitdate))))) x))))))) (let* ([key (hash-ref s 's_name)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 's s 'l1 l1 'o o 'n n) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (list (- (length (hash-ref g 'items))) (hash-ref g 'key)))) (string>? (let ([g a]) (list (- (length (hash-ref g 'items))) (hash-ref g 'key))) (let ([g b]) (list (- (length (hash-ref g 'items))) (hash-ref g 'key))))] [(string? (let ([g b]) (list (- (length (hash-ref g 'items))) (hash-ref g 'key)))) (string>? (let ([g a]) (list (- (length (hash-ref g 'items))) (hash-ref g 'key))) (let ([g b]) (list (- (length (hash-ref g 'items))) (hash-ref g 'key))))] [else (> (let ([g a]) (list (- (length (hash-ref g 'items))) (hash-ref g 'key))) (let ([g b]) (list (- (length (hash-ref g 'items))) (hash-ref g 'key))))]))))
  (for/list ([g _groups]) (hash 's_name (hash-ref g 'key) 'numwait (length (hash-ref g 'items))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 's_name "Desert Trade" 'numwait 1))) (displayln "ok"))
