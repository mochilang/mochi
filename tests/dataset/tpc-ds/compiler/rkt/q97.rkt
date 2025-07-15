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

(struct StoreSale (ss_customer_sk ss_item_sk) #:transparent #:mutable)
(struct CatalogSale (cs_bill_customer_sk cs_item_sk) #:transparent #:mutable)
(define store_sales (list (hash 'ss_customer_sk 1 'ss_item_sk 1) (hash 'ss_customer_sk 2 'ss_item_sk 1)))
(define catalog_sales (list (hash 'cs_bill_customer_sk 1 'cs_item_sk 1) (hash 'cs_bill_customer_sk 3 'cs_item_sk 2)))
(define ssci (let ([groups (make-hash)])
  (for* ([ss store_sales]) (let* ([key (hash 'customer_sk (hash-ref ss 'ss_customer_sk) 'item_sk (hash-ref ss 'ss_item_sk))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons ss bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'customer_sk (hash-ref (hash-ref g 'key) 'customer_sk) 'item_sk (hash-ref (hash-ref g 'key) 'item_sk)))))
(define csci (let ([groups (make-hash)])
  (for* ([cs catalog_sales]) (let* ([key (hash 'customer_sk (hash-ref cs 'cs_bill_customer_sk) 'item_sk (hash-ref cs 'cs_item_sk))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons cs bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'customer_sk (hash-ref (hash-ref g 'key) 'customer_sk) 'item_sk (hash-ref (hash-ref g 'key) 'item_sk)))))
(define both (if (and (hash? (for*/list ([s ssci] [c csci] #:when (and (and (equal? (hash-ref s 'customer_sk) (hash-ref c 'customer_sk)) (equal? (hash-ref s 'item_sk) (hash-ref c 'item_sk))))) 1)) (hash-has-key? (for*/list ([s ssci] [c csci] #:when (and (and (equal? (hash-ref s 'customer_sk) (hash-ref c 'customer_sk)) (equal? (hash-ref s 'item_sk) (hash-ref c 'item_sk))))) 1) 'items)) (length (hash-ref (for*/list ([s ssci] [c csci] #:when (and (and (equal? (hash-ref s 'customer_sk) (hash-ref c 'customer_sk)) (equal? (hash-ref s 'item_sk) (hash-ref c 'item_sk))))) 1) 'items)) (length (for*/list ([s ssci] [c csci] #:when (and (and (equal? (hash-ref s 'customer_sk) (hash-ref c 'customer_sk)) (equal? (hash-ref s 'item_sk) (hash-ref c 'item_sk))))) 1))))
(define store_only (if (and (hash? (for*/list ([s ssci] #:when (and (not (not (null? (for*/list ([c csci] #:when (and (and (equal? (hash-ref s 'customer_sk) (hash-ref c 'customer_sk)) (equal? (hash-ref s 'item_sk) (hash-ref c 'item_sk))))) c)))))) 1)) (hash-has-key? (for*/list ([s ssci] #:when (and (not (not (null? (for*/list ([c csci] #:when (and (and (equal? (hash-ref s 'customer_sk) (hash-ref c 'customer_sk)) (equal? (hash-ref s 'item_sk) (hash-ref c 'item_sk))))) c)))))) 1) 'items)) (length (hash-ref (for*/list ([s ssci] #:when (and (not (not (null? (for*/list ([c csci] #:when (and (and (equal? (hash-ref s 'customer_sk) (hash-ref c 'customer_sk)) (equal? (hash-ref s 'item_sk) (hash-ref c 'item_sk))))) c)))))) 1) 'items)) (length (for*/list ([s ssci] #:when (and (not (not (null? (for*/list ([c csci] #:when (and (and (equal? (hash-ref s 'customer_sk) (hash-ref c 'customer_sk)) (equal? (hash-ref s 'item_sk) (hash-ref c 'item_sk))))) c)))))) 1))))
(define catalog_only (if (and (hash? (for*/list ([c csci] #:when (and (not (not (null? (for*/list ([s ssci] #:when (and (and (equal? (hash-ref s 'customer_sk) (hash-ref c 'customer_sk)) (equal? (hash-ref s 'item_sk) (hash-ref c 'item_sk))))) s)))))) 1)) (hash-has-key? (for*/list ([c csci] #:when (and (not (not (null? (for*/list ([s ssci] #:when (and (and (equal? (hash-ref s 'customer_sk) (hash-ref c 'customer_sk)) (equal? (hash-ref s 'item_sk) (hash-ref c 'item_sk))))) s)))))) 1) 'items)) (length (hash-ref (for*/list ([c csci] #:when (and (not (not (null? (for*/list ([s ssci] #:when (and (and (equal? (hash-ref s 'customer_sk) (hash-ref c 'customer_sk)) (equal? (hash-ref s 'item_sk) (hash-ref c 'item_sk))))) s)))))) 1) 'items)) (length (for*/list ([c csci] #:when (and (not (not (null? (for*/list ([s ssci] #:when (and (and (equal? (hash-ref s 'customer_sk) (hash-ref c 'customer_sk)) (equal? (hash-ref s 'item_sk) (hash-ref c 'item_sk))))) s)))))) 1))))
(define result (hash 'store_only store_only 'catalog_only catalog_only 'store_and_catalog both))
(displayln (jsexpr->string (_json-fix result)))
(when (and (and (equal? (hash-ref result 'store_only) 1) (equal? (hash-ref result 'catalog_only) 1)) (equal? (hash-ref result 'store_and_catalog) 1)) (displayln "ok"))
