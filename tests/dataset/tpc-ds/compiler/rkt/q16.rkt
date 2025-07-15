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

(struct CatalogSale (cs_order_number cs_ship_date_sk cs_ship_addr_sk cs_call_center_sk cs_warehouse_sk cs_ext_ship_cost cs_net_profit) #:transparent #:mutable)
(struct DateDim (d_date_sk d_date) #:transparent #:mutable)
(struct CustomerAddress (ca_address_sk ca_state) #:transparent #:mutable)
(struct CallCenter (cc_call_center_sk cc_county) #:transparent #:mutable)
(struct CatalogReturn (cr_order_number) #:transparent #:mutable)
(define catalog_sales (list (hash 'cs_order_number 1 'cs_ship_date_sk 1 'cs_ship_addr_sk 1 'cs_call_center_sk 1 'cs_warehouse_sk 1 'cs_ext_ship_cost 5 'cs_net_profit 20) (hash 'cs_order_number 1 'cs_ship_date_sk 1 'cs_ship_addr_sk 1 'cs_call_center_sk 1 'cs_warehouse_sk 2 'cs_ext_ship_cost 0 'cs_net_profit 0)))
(define date_dim (list (hash 'd_date_sk 1 'd_date "2000-03-01")))
(define customer_address (list (hash 'ca_address_sk 1 'ca_state "CA")))
(define call_center (list (hash 'cc_call_center_sk 1 'cc_county "CountyA")))
(define catalog_returns '())
(define (distinct xs)
  (let/ec return
(define out '())
(for ([x (if (hash? xs) (hash-keys xs) xs)])
(if (not (contains out x))
  (begin
(set! out (append out (list x)))
  )
  (void)
)
)
(return out)
  ))
(define filtered (let ([groups (make-hash)])
  (for* ([cs1 catalog_sales] [d date_dim] [ca customer_address] [cc call_center] #:when (and (and (and (equal? (hash-ref cs1 'cs_ship_date_sk) (hash-ref d 'd_date_sk)) (string>=? (hash-ref d 'd_date) "2000-03-01")) (string<=? (hash-ref d 'd_date) "2000-04-30")) (and (equal? (hash-ref cs1 'cs_ship_addr_sk) (hash-ref ca 'ca_address_sk)) (string=? (hash-ref ca 'ca_state) "CA")) (and (equal? (hash-ref cs1 'cs_call_center_sk) (hash-ref cc 'cc_call_center_sk)) (string=? (hash-ref cc 'cc_county) "CountyA")) (and (not (null? (for*/list ([cs2 catalog_sales] #:when (and (and (equal? (hash-ref cs1 'cs_order_number) (hash-ref cs2 'cs_order_number)) (not (equal? (hash-ref cs1 'cs_warehouse_sk) (hash-ref cs2 'cs_warehouse_sk)))))) cs2))) (equal? (not (null? (for*/list ([cr catalog_returns] #:when (and (equal? (hash-ref cs1 'cs_order_number) (hash-ref cr 'cr_order_number)))) cr))) #f)))) (let* ([key (hash )] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'cs1 cs1 'd d 'ca ca 'cc cc) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'order_count (cond [(string? (distinct (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_order_number)))) (string-length (distinct (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_order_number))))] [(hash? (distinct (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_order_number)))) (hash-count (distinct (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_order_number))))] [else (length (distinct (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_order_number))))]) 'total_shipping_cost (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_ext_ship_cost))]) v)) 'total_net_profit (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cs_net_profit))]) v))))))
(displayln (jsexpr->string (_json-fix filtered)))
(when (equal? filtered (list (hash 'order_count 1 'total_shipping_cost 5 'total_net_profit 20))) (displayln "ok"))
