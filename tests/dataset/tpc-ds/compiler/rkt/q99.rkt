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

(struct CatalogSale (cs_ship_date_sk cs_sold_date_sk cs_warehouse_sk cs_ship_mode_sk cs_call_center_sk) #:transparent #:mutable)
(struct Warehouse (w_warehouse_sk w_warehouse_name) #:transparent #:mutable)
(struct ShipMode (sm_ship_mode_sk sm_type) #:transparent #:mutable)
(struct CallCenter (cc_call_center_sk cc_name) #:transparent #:mutable)
(define catalog_sales (list (hash 'cs_ship_date_sk 31 'cs_sold_date_sk 1 'cs_warehouse_sk 1 'cs_ship_mode_sk 1 'cs_call_center_sk 1) (hash 'cs_ship_date_sk 51 'cs_sold_date_sk 1 'cs_warehouse_sk 1 'cs_ship_mode_sk 1 'cs_call_center_sk 1) (hash 'cs_ship_date_sk 71 'cs_sold_date_sk 1 'cs_warehouse_sk 1 'cs_ship_mode_sk 1 'cs_call_center_sk 1) (hash 'cs_ship_date_sk 101 'cs_sold_date_sk 1 'cs_warehouse_sk 1 'cs_ship_mode_sk 1 'cs_call_center_sk 1) (hash 'cs_ship_date_sk 131 'cs_sold_date_sk 1 'cs_warehouse_sk 1 'cs_ship_mode_sk 1 'cs_call_center_sk 1)))
(define warehouse (list (hash 'w_warehouse_sk 1 'w_warehouse_name "Warehouse1")))
(define ship_mode (list (hash 'sm_ship_mode_sk 1 'sm_type "EXP")))
(define call_center (list (hash 'cc_call_center_sk 1 'cc_name "CC1")))
(define grouped (let ([groups (make-hash)])
  (for* ([cs catalog_sales] [w warehouse] [sm ship_mode] [cc call_center] #:when (and (equal? (hash-ref cs 'cs_warehouse_sk) (hash-ref w 'w_warehouse_sk)) (equal? (hash-ref cs 'cs_ship_mode_sk) (hash-ref sm 'sm_ship_mode_sk)) (equal? (hash-ref cs 'cs_call_center_sk) (hash-ref cc 'cc_call_center_sk)))) (let* ([key (hash 'warehouse (substr (hash-ref w 'w_warehouse_name) 0 20) 'sm_type (hash-ref sm 'sm_type) 'cc_name (hash-ref cc 'cc_name))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'cs cs 'w w 'sm sm 'cc cc) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'warehouse (hash-ref (hash-ref g 'key) 'warehouse) 'sm_type (hash-ref (hash-ref g 'key) 'sm_type) 'cc_name (hash-ref (hash-ref g 'key) 'cc_name) 'd30 (if (and (hash? (for*/list ([x (hash-ref g 'items)] #:when (and (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 30))) x)) (hash-has-key? (for*/list ([x (hash-ref g 'items)] #:when (and (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 30))) x) 'items)) (length (hash-ref (for*/list ([x (hash-ref g 'items)] #:when (and (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 30))) x) 'items)) (length (for*/list ([x (hash-ref g 'items)] #:when (and (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 30))) x))) 'd60 (if (and (hash? (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 30) (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 60)))) x)) (hash-has-key? (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 30) (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 60)))) x) 'items)) (length (hash-ref (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 30) (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 60)))) x) 'items)) (length (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 30) (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 60)))) x))) 'd90 (if (and (hash? (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 60) (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 90)))) x)) (hash-has-key? (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 60) (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 90)))) x) 'items)) (length (hash-ref (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 60) (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 90)))) x) 'items)) (length (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 60) (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 90)))) x))) 'd120 (if (and (hash? (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 90) (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 120)))) x)) (hash-has-key? (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 90) (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 120)))) x) 'items)) (length (hash-ref (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 90) (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 120)))) x) 'items)) (length (for*/list ([x (hash-ref g 'items)] #:when (and (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 90) (_le (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 120)))) x))) 'dmore (if (and (hash? (for*/list ([x (hash-ref g 'items)] #:when (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 120))) x)) (hash-has-key? (for*/list ([x (hash-ref g 'items)] #:when (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 120))) x) 'items)) (length (hash-ref (for*/list ([x (hash-ref g 'items)] #:when (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 120))) x) 'items)) (length (for*/list ([x (hash-ref g 'items)] #:when (and (_gt (- (hash-ref x 'cs_ship_date_sk) (hash-ref x 'cs_sold_date_sk)) 120))) x)))))))
(displayln (jsexpr->string (_json-fix grouped)))
(when (equal? grouped (list (hash 'warehouse "Warehouse1" 'sm_type "EXP" 'cc_name "CC1" 'd30 1 'd60 1 'd90 1 'd120 1 'dmore 1))) (displayln "ok"))
