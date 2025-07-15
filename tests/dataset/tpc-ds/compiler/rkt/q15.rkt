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

(struct CatalogSale (cs_bill_customer_sk cs_sales_price cs_sold_date_sk) #:transparent #:mutable)
(struct Customer (c_customer_sk c_current_addr_sk) #:transparent #:mutable)
(struct CustomerAddress (ca_address_sk ca_zip ca_state) #:transparent #:mutable)
(struct DateDim (d_date_sk d_qoy d_year) #:transparent #:mutable)
(define catalog_sales (list (hash 'cs_bill_customer_sk 1 'cs_sales_price 600 'cs_sold_date_sk 1)))
(define customer (list (hash 'c_customer_sk 1 'c_current_addr_sk 1)))
(define customer_address (list (hash 'ca_address_sk 1 'ca_zip "85669" 'ca_state "CA")))
(define date_dim (list (hash 'd_date_sk 1 'd_qoy 1 'd_year 2000)))
(define filtered (let ([groups (make-hash)])
  (for* ([cs catalog_sales] [c customer] [ca customer_address] [d date_dim] #:when (and (equal? (hash-ref cs 'cs_bill_customer_sk) (hash-ref c 'c_customer_sk)) (equal? (hash-ref c 'c_current_addr_sk) (hash-ref ca 'ca_address_sk)) (equal? (hash-ref cs 'cs_sold_date_sk) (hash-ref d 'd_date_sk)) (and (and (or (or (cond [(string? '("85669" "86197" "88274" "83405" "86475" "85392" "85460" "80348" "81792")) (regexp-match? (regexp (substr (hash-ref ca 'ca_zip) 0 5)) '("85669" "86197" "88274" "83405" "86475" "85392" "85460" "80348" "81792"))] [(hash? '("85669" "86197" "88274" "83405" "86475" "85392" "85460" "80348" "81792")) (hash-has-key? '("85669" "86197" "88274" "83405" "86475" "85392" "85460" "80348" "81792") (substr (hash-ref ca 'ca_zip) 0 5))] [else (member (substr (hash-ref ca 'ca_zip) 0 5) '("85669" "86197" "88274" "83405" "86475" "85392" "85460" "80348" "81792"))]) (cond [(string? '("CA" "WA" "GA")) (regexp-match? (regexp (hash-ref ca 'ca_state)) '("CA" "WA" "GA"))] [(hash? '("CA" "WA" "GA")) (hash-has-key? '("CA" "WA" "GA") (hash-ref ca 'ca_state))] [else (member (hash-ref ca 'ca_state) '("CA" "WA" "GA"))])) (_gt (hash-ref cs 'cs_sales_price) 500)) (equal? (hash-ref d 'd_qoy) 1)) (equal? (hash-ref d 'd_year) 2000)))) (let* ([key (hash 'zip (hash-ref ca 'ca_zip))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'cs cs 'c c 'ca ca 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (hash-ref (hash-ref g 'key) 'zip))) (string>? (let ([g a]) (hash-ref (hash-ref g 'key) 'zip)) (let ([g b]) (hash-ref (hash-ref g 'key) 'zip)))] [(string? (let ([g b]) (hash-ref (hash-ref g 'key) 'zip))) (string>? (let ([g a]) (hash-ref (hash-ref g 'key) 'zip)) (let ([g b]) (hash-ref (hash-ref g 'key) 'zip)))] [else (> (let ([g a]) (hash-ref (hash-ref g 'key) 'zip)) (let ([g b]) (hash-ref (hash-ref g 'key) 'zip)))]))))
  (for/list ([g _groups]) (hash 'ca_zip (hash-ref (hash-ref g 'key) 'zip) 'sum_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_sales_price))]) v))))))
(displayln (jsexpr->string (_json-fix filtered)))
(when (equal? filtered (list (hash 'ca_zip "85669" 'sum_sales 600))) (displayln "ok"))
