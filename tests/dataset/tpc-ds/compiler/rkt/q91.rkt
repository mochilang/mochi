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

(struct CallCenter (cc_call_center_sk cc_call_center_id cc_name cc_manager) #:transparent #:mutable)
(struct CatalogReturn (cr_call_center_sk cr_returned_date_sk cr_returning_customer_sk cr_net_loss) #:transparent #:mutable)
(struct DateDim (d_date_sk d_year d_moy) #:transparent #:mutable)
(struct Customer (c_customer_sk c_current_cdemo_sk c_current_hdemo_sk c_current_addr_sk) #:transparent #:mutable)
(struct CustomerAddress (ca_address_sk ca_gmt_offset) #:transparent #:mutable)
(struct CustomerDemographics (cd_demo_sk cd_marital_status cd_education_status) #:transparent #:mutable)
(struct HouseholdDemographics (hd_demo_sk hd_buy_potential) #:transparent #:mutable)
(define call_center (list (hash 'cc_call_center_sk 1 'cc_call_center_id "CC1" 'cc_name "Main" 'cc_manager "Alice")))
(define catalog_returns (list (hash 'cr_call_center_sk 1 'cr_returned_date_sk 1 'cr_returning_customer_sk 1 'cr_net_loss 10)))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2001 'd_moy 5)))
(define customer (list (hash 'c_customer_sk 1 'c_current_cdemo_sk 1 'c_current_hdemo_sk 1 'c_current_addr_sk 1)))
(define customer_demographics (list (hash 'cd_demo_sk 1 'cd_marital_status "M" 'cd_education_status "Unknown")))
(define household_demographics (list (hash 'hd_demo_sk 1 'hd_buy_potential "1001-5000")))
(define customer_address (list (hash 'ca_address_sk 1 'ca_gmt_offset (- 6))))
(define result (first (let ([groups (make-hash)])
  (for* ([cc call_center] [cr catalog_returns] [d date_dim] [c customer] [cd customer_demographics] [hd household_demographics] [ca customer_address] #:when (and (equal? (hash-ref cc 'cc_call_center_sk) (hash-ref cr 'cr_call_center_sk)) (equal? (hash-ref cr 'cr_returned_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref cr 'cr_returning_customer_sk) (hash-ref c 'c_customer_sk)) (equal? (hash-ref c 'c_current_cdemo_sk) (hash-ref cd 'cd_demo_sk)) (equal? (hash-ref c 'c_current_hdemo_sk) (hash-ref hd 'hd_demo_sk)) (equal? (hash-ref c 'c_current_addr_sk) (hash-ref ca 'ca_address_sk)) (and (and (and (and (and (equal? (hash-ref d 'd_year) 2001) (equal? (hash-ref d 'd_moy) 5)) (string=? (hash-ref cd 'cd_marital_status) "M")) (string=? (hash-ref cd 'cd_education_status) "Unknown")) (string=? (hash-ref hd 'hd_buy_potential) "1001-5000")) (equal? (hash-ref ca 'ca_gmt_offset) (- 6))))) (let* ([key (hash 'id (hash-ref cc 'cc_call_center_id) 'name (hash-ref cc 'cc_name) 'mgr (hash-ref cc 'cc_manager))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'cc cc 'cr cr 'd d 'c c 'cd cd 'hd hd 'ca ca) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'Call_Center (hash-ref (hash-ref g 'key) 'id) 'Call_Center_Name (hash-ref (hash-ref g 'key) 'name) 'Manager (hash-ref (hash-ref g 'key) 'mgr) 'Returns_Loss (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'cr_net_loss))]) v)))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (hash 'Call_Center "CC1" 'Call_Center_Name "Main" 'Manager "Alice" 'Returns_Loss 10)) (displayln "ok"))
