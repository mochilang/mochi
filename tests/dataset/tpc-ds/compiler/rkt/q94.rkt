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

(struct WebSale (ws_order_number ws_ship_date_sk ws_warehouse_sk ws_ship_addr_sk ws_web_site_sk ws_net_profit ws_ext_ship_cost) #:transparent #:mutable)
(struct WebReturn (wr_order_number) #:transparent #:mutable)
(struct DateDim (d_date_sk d_date) #:transparent #:mutable)
(struct CustomerAddress (ca_address_sk ca_state) #:transparent #:mutable)
(struct WebSite (web_site_sk web_company_name) #:transparent #:mutable)
(define web_sales (list (hash 'ws_order_number 1 'ws_ship_date_sk 1 'ws_warehouse_sk 1 'ws_ship_addr_sk 1 'ws_web_site_sk 1 'ws_net_profit 5 'ws_ext_ship_cost 2) (hash 'ws_order_number 1 'ws_ship_date_sk 1 'ws_warehouse_sk 2 'ws_ship_addr_sk 1 'ws_web_site_sk 1 'ws_net_profit 0 'ws_ext_ship_cost 0) (hash 'ws_order_number 2 'ws_ship_date_sk 1 'ws_warehouse_sk 3 'ws_ship_addr_sk 1 'ws_web_site_sk 1 'ws_net_profit 3 'ws_ext_ship_cost 1)))
(define web_returns (list (hash 'wr_order_number 2)))
(define date_dim (list (hash 'd_date_sk 1 'd_date "2001-02-01")))
(define customer_address (list (hash 'ca_address_sk 1 'ca_state "CA")))
(define web_site (list (hash 'web_site_sk 1 'web_company_name "pri")))
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
(define filtered (for*/list ([ws web_sales] [d date_dim] [ca customer_address] [w web_site] #:when (and (equal? (hash-ref ws 'ws_ship_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref ws 'ws_ship_addr_sk) (hash-ref ca 'ca_address_sk)) (equal? (hash-ref ws 'ws_web_site_sk) (hash-ref w 'web_site_sk)) (and (and (and (string=? (hash-ref ca 'ca_state) "CA") (string=? (hash-ref w 'web_company_name) "pri")) (not (null? (for*/list ([ws2 web_sales] #:when (and (and (equal? (hash-ref ws 'ws_order_number) (hash-ref ws2 'ws_order_number)) (not (equal? (hash-ref ws 'ws_warehouse_sk) (hash-ref ws2 'ws_warehouse_sk)))))) ws2)))) (equal? (not (null? (for*/list ([wr web_returns] #:when (and (equal? (hash-ref wr 'wr_order_number) (hash-ref ws 'ws_order_number)))) wr))) #f)))) ws))
(define result (hash 'order_count (cond [(string? (distinct (for*/list ([x filtered]) (hash-ref x 'ws_order_number)))) (string-length (distinct (for*/list ([x filtered]) (hash-ref x 'ws_order_number))))] [(hash? (distinct (for*/list ([x filtered]) (hash-ref x 'ws_order_number)))) (hash-count (distinct (for*/list ([x filtered]) (hash-ref x 'ws_order_number))))] [else (length (distinct (for*/list ([x filtered]) (hash-ref x 'ws_order_number))))]) 'total_shipping_cost (apply + (for*/list ([v (for*/list ([x filtered]) (hash-ref x 'ws_ext_ship_cost))]) v)) 'total_net_profit (apply + (for*/list ([v (for*/list ([x filtered]) (hash-ref x 'ws_net_profit))]) v))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (hash 'order_count 1 'total_shipping_cost 2 'total_net_profit 5)) (displayln "ok"))
