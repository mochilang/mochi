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

(define orders (list (hash 'o_orderkey 1 'o_orderpriority "1-URGENT") (hash 'o_orderkey 2 'o_orderpriority "3-MEDIUM")))
(define lineitem (list (hash 'l_orderkey 1 'l_shipmode "MAIL" 'l_commitdate "1994-02-10" 'l_receiptdate "1994-02-15" 'l_shipdate "1994-02-05") (hash 'l_orderkey 2 'l_shipmode "SHIP" 'l_commitdate "1994-03-01" 'l_receiptdate "1994-02-28" 'l_shipdate "1994-02-27")))
(define result (let ([groups (make-hash)])
  (for* ([l lineitem] [o orders] #:when (and (equal? (hash-ref o 'o_orderkey) (hash-ref l 'l_orderkey)) (and (and (and (and (cond [(string? '("MAIL" "SHIP")) (regexp-match? (regexp (hash-ref l 'l_shipmode)) '("MAIL" "SHIP"))] [(hash? '("MAIL" "SHIP")) (hash-has-key? '("MAIL" "SHIP") (hash-ref l 'l_shipmode))] [else (member (hash-ref l 'l_shipmode) '("MAIL" "SHIP"))]) (_lt (hash-ref l 'l_commitdate) (hash-ref l 'l_receiptdate))) (_lt (hash-ref l 'l_shipdate) (hash-ref l 'l_commitdate))) (string>=? (hash-ref l 'l_receiptdate) "1994-01-01")) (string<? (hash-ref l 'l_receiptdate) "1995-01-01")))) (let* ([key (hash-ref l 'l_shipmode)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'l l 'o o) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (hash-ref g 'key))) (string>? (let ([g a]) (hash-ref g 'key)) (let ([g b]) (hash-ref g 'key)))] [(string? (let ([g b]) (hash-ref g 'key))) (string>? (let ([g a]) (hash-ref g 'key)) (let ([g b]) (hash-ref g 'key)))] [else (> (let ([g a]) (hash-ref g 'key)) (let ([g b]) (hash-ref g 'key)))]))))
  (for/list ([g _groups]) (hash 'l_shipmode (hash-ref g 'key) 'high_line_count (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (if (cond [(string? '("1-URGENT" "2-HIGH")) (regexp-match? (regexp (hash-ref (hash-ref x 'o) 'o_orderpriority)) '("1-URGENT" "2-HIGH"))] [(hash? '("1-URGENT" "2-HIGH")) (hash-has-key? '("1-URGENT" "2-HIGH") (hash-ref (hash-ref x 'o) 'o_orderpriority))] [else (member (hash-ref (hash-ref x 'o) 'o_orderpriority) '("1-URGENT" "2-HIGH"))]) 1 0))]) v)) 'low_line_count (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (if (not (cond [(string? '("1-URGENT" "2-HIGH")) (regexp-match? (regexp (hash-ref (hash-ref x 'o) 'o_orderpriority)) '("1-URGENT" "2-HIGH"))] [(hash? '("1-URGENT" "2-HIGH")) (hash-has-key? '("1-URGENT" "2-HIGH") (hash-ref (hash-ref x 'o) 'o_orderpriority))] [else (member (hash-ref (hash-ref x 'o) 'o_orderpriority) '("1-URGENT" "2-HIGH"))])) 1 0))]) v))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'l_shipmode "MAIL" 'high_line_count 1 'low_line_count 0))) (displayln "ok"))
