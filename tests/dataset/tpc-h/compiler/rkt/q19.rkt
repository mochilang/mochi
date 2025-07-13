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

(define part (list (hash 'p_partkey 1 'p_brand "Brand#12" 'p_container "SM BOX" 'p_size 3) (hash 'p_partkey 2 'p_brand "Brand#23" 'p_container "MED BOX" 'p_size 5) (hash 'p_partkey 3 'p_brand "Brand#34" 'p_container "LG BOX" 'p_size 15)))
(define lineitem (list (hash 'l_partkey 1 'l_quantity 5 'l_extendedprice 1000 'l_discount 0.1 'l_shipmode "AIR" 'l_shipinstruct "DELIVER IN PERSON") (hash 'l_partkey 2 'l_quantity 15 'l_extendedprice 2000 'l_discount 0.05 'l_shipmode "AIR REG" 'l_shipinstruct "DELIVER IN PERSON") (hash 'l_partkey 3 'l_quantity 35 'l_extendedprice 1500 'l_discount 0 'l_shipmode "AIR" 'l_shipinstruct "DELIVER IN PERSON")))
(define revenues (for*/list ([l lineitem] [p part] #:when (and (equal? (hash-ref p 'p_partkey) (hash-ref l 'l_partkey)) (and (and (or (or (and (and (and (string=? (hash-ref p 'p_brand) "Brand#12") (cond [(string? '("SM CASE" "SM BOX" "SM PACK" "SM PKG")) (regexp-match? (regexp (hash-ref p 'p_container)) '("SM CASE" "SM BOX" "SM PACK" "SM PKG"))] [(hash? '("SM CASE" "SM BOX" "SM PACK" "SM PKG")) (hash-has-key? '("SM CASE" "SM BOX" "SM PACK" "SM PKG") (hash-ref p 'p_container))] [else (member (hash-ref p 'p_container) '("SM CASE" "SM BOX" "SM PACK" "SM PKG"))])) (and (_ge (hash-ref l 'l_quantity) 1) (_le (hash-ref l 'l_quantity) 11))) (and (_ge (hash-ref p 'p_size) 1) (_le (hash-ref p 'p_size) 5))) (and (and (and (string=? (hash-ref p 'p_brand) "Brand#23") (cond [(string? '("MED BAG" "MED BOX" "MED PKG" "MED PACK")) (regexp-match? (regexp (hash-ref p 'p_container)) '("MED BAG" "MED BOX" "MED PKG" "MED PACK"))] [(hash? '("MED BAG" "MED BOX" "MED PKG" "MED PACK")) (hash-has-key? '("MED BAG" "MED BOX" "MED PKG" "MED PACK") (hash-ref p 'p_container))] [else (member (hash-ref p 'p_container) '("MED BAG" "MED BOX" "MED PKG" "MED PACK"))])) (and (_ge (hash-ref l 'l_quantity) 10) (_le (hash-ref l 'l_quantity) 20))) (and (_ge (hash-ref p 'p_size) 1) (_le (hash-ref p 'p_size) 10)))) (and (and (and (string=? (hash-ref p 'p_brand) "Brand#34") (cond [(string? '("LG CASE" "LG BOX" "LG PACK" "LG PKG")) (regexp-match? (regexp (hash-ref p 'p_container)) '("LG CASE" "LG BOX" "LG PACK" "LG PKG"))] [(hash? '("LG CASE" "LG BOX" "LG PACK" "LG PKG")) (hash-has-key? '("LG CASE" "LG BOX" "LG PACK" "LG PKG") (hash-ref p 'p_container))] [else (member (hash-ref p 'p_container) '("LG CASE" "LG BOX" "LG PACK" "LG PKG"))])) (and (_ge (hash-ref l 'l_quantity) 20) (_le (hash-ref l 'l_quantity) 30))) (and (_ge (hash-ref p 'p_size) 1) (_le (hash-ref p 'p_size) 15)))) (cond [(string? '("AIR" "AIR REG")) (regexp-match? (regexp (hash-ref l 'l_shipmode)) '("AIR" "AIR REG"))] [(hash? '("AIR" "AIR REG")) (hash-has-key? '("AIR" "AIR REG") (hash-ref l 'l_shipmode))] [else (member (hash-ref l 'l_shipmode) '("AIR" "AIR REG"))])) (string=? (hash-ref l 'l_shipinstruct) "DELIVER IN PERSON")))) (* (hash-ref l 'l_extendedprice) (- 1 (hash-ref l 'l_discount)))))
(define result (apply + (for*/list ([v revenues]) v)))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result 2800) (displayln "ok"))
