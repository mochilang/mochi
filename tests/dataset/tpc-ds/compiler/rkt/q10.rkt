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

(struct Customer (c_customer_sk c_current_addr_sk c_current_cdemo_sk) #:transparent #:mutable)
(struct CustomerAddress (ca_address_sk ca_county) #:transparent #:mutable)
(struct CustomerDemographics (cd_demo_sk cd_gender cd_marital_status cd_education_status cd_purchase_estimate cd_credit_rating cd_dep_count cd_dep_employed_count cd_dep_college_count) #:transparent #:mutable)
(struct StoreSale (ss_customer_sk ss_sold_date_sk) #:transparent #:mutable)
(struct DateDim (d_date_sk d_year d_moy) #:transparent #:mutable)
(define customer (list (hash 'c_customer_sk 1 'c_current_addr_sk 1 'c_current_cdemo_sk 1)))
(define customer_address (list (hash 'ca_address_sk 1 'ca_county "CountyA")))
(define customer_demographics (list (hash 'cd_demo_sk 1 'cd_gender "F" 'cd_marital_status "M" 'cd_education_status "College" 'cd_purchase_estimate 5000 'cd_credit_rating "Good" 'cd_dep_count 1 'cd_dep_employed_count 1 'cd_dep_college_count 0)))
(define store_sales (list (hash 'ss_customer_sk 1 'ss_sold_date_sk 1)))
(define web_sales '())
(define catalog_sales '())
(define date_dim (list (hash 'd_date_sk 1 'd_year 2000 'd_moy 2)))
(define active (for*/list ([c customer] [ca customer_address] [cd customer_demographics] #:when (and (and (equal? (hash-ref c 'c_current_addr_sk) (hash-ref ca 'ca_address_sk)) (string=? (hash-ref ca 'ca_county) "CountyA")) (equal? (hash-ref c 'c_current_cdemo_sk) (hash-ref cd 'cd_demo_sk)) (not (null? (for*/list ([ss store_sales] [d date_dim] #:when (and (equal? (hash-ref ss 'ss_sold_date_sk) (hash-ref d 'd_date_sk)) (and (and (and (equal? (hash-ref ss 'ss_customer_sk) (hash-ref c 'c_customer_sk)) (equal? (hash-ref d 'd_year) 2000)) (_ge (hash-ref d 'd_moy) 2)) (_le (hash-ref d 'd_moy) 5)))) ss))))) cd))
(define result (let ([groups (make-hash)])
  (for* ([a active]) (let* ([key (hash 'gender (hash-ref a 'cd_gender) 'marital (hash-ref a 'cd_marital_status) 'education (hash-ref a 'cd_education_status) 'purchase (hash-ref a 'cd_purchase_estimate) 'credit (hash-ref a 'cd_credit_rating) 'dep (hash-ref a 'cd_dep_count) 'depemp (hash-ref a 'cd_dep_employed_count) 'depcol (hash-ref a 'cd_dep_college_count))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons a bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'cd_gender (hash-ref (hash-ref g 'key) 'gender) 'cd_marital_status (hash-ref (hash-ref g 'key) 'marital) 'cd_education_status (hash-ref (hash-ref g 'key) 'education) 'cnt1 (if (and (hash? (for*/list ([_ (hash-ref g 'items)]) _)) (hash-has-key? (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (hash-ref (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (for*/list ([_ (hash-ref g 'items)]) _))) 'cd_purchase_estimate (hash-ref (hash-ref g 'key) 'purchase) 'cnt2 (if (and (hash? (for*/list ([_ (hash-ref g 'items)]) _)) (hash-has-key? (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (hash-ref (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (for*/list ([_ (hash-ref g 'items)]) _))) 'cd_credit_rating (hash-ref (hash-ref g 'key) 'credit) 'cnt3 (if (and (hash? (for*/list ([_ (hash-ref g 'items)]) _)) (hash-has-key? (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (hash-ref (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (for*/list ([_ (hash-ref g 'items)]) _))) 'cd_dep_count (hash-ref (hash-ref g 'key) 'dep) 'cnt4 (if (and (hash? (for*/list ([_ (hash-ref g 'items)]) _)) (hash-has-key? (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (hash-ref (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (for*/list ([_ (hash-ref g 'items)]) _))) 'cd_dep_employed_count (hash-ref (hash-ref g 'key) 'depemp) 'cnt5 (if (and (hash? (for*/list ([_ (hash-ref g 'items)]) _)) (hash-has-key? (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (hash-ref (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (for*/list ([_ (hash-ref g 'items)]) _))) 'cd_dep_college_count (hash-ref (hash-ref g 'key) 'depcol) 'cnt6 (if (and (hash? (for*/list ([_ (hash-ref g 'items)]) _)) (hash-has-key? (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (hash-ref (for*/list ([_ (hash-ref g 'items)]) _) 'items)) (length (for*/list ([_ (hash-ref g 'items)]) _)))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'cd_gender "F" 'cd_marital_status "M" 'cd_education_status "College" 'cnt1 1 'cd_purchase_estimate 5000 'cnt2 1 'cd_credit_rating "Good" 'cnt3 1 'cd_dep_count 1 'cnt4 1 'cd_dep_employed_count 1 'cnt5 1 'cd_dep_college_count 0 'cnt6 1))) (displayln "ok"))
