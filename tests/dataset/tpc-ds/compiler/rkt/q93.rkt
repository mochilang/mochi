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

(struct StoreSale (ss_item_sk ss_ticket_number ss_customer_sk ss_quantity ss_sales_price) #:transparent #:mutable)
(struct StoreReturn (sr_item_sk sr_ticket_number sr_reason_sk sr_return_quantity) #:transparent #:mutable)
(struct Reason (r_reason_sk r_reason_desc) #:transparent #:mutable)
(define store_sales (list (hash 'ss_item_sk 1 'ss_ticket_number 1 'ss_customer_sk 1 'ss_quantity 5 'ss_sales_price 10) (hash 'ss_item_sk 1 'ss_ticket_number 2 'ss_customer_sk 2 'ss_quantity 3 'ss_sales_price 20)))
(define store_returns (list (hash 'sr_item_sk 1 'sr_ticket_number 1 'sr_reason_sk 1 'sr_return_quantity 1)))
(define reason (list (hash 'r_reason_sk 1 'r_reason_desc "ReasonA")))
(define t (for*/list ([ss store_sales] #:when (and (or (equal? r '()) (string=? (hash-ref r 'r_reason_desc) "ReasonA")))) (let ((sr (findf (lambda (sr) (and (equal? (hash-ref ss 'ss_item_sk) (hash-ref sr 'sr_item_sk)) (equal? (hash-ref ss 'ss_ticket_number) (hash-ref sr 'sr_ticket_number)))) store_returns)) (r (findf (lambda (r) (and (not (equal? sr '())) (equal? (hash-ref sr 'sr_reason_sk) (hash-ref r 'r_reason_sk)))) reason))) (hash 'ss_customer_sk (hash-ref ss 'ss_customer_sk) 'act_sales (if (not (equal? sr '())) (* (- (hash-ref ss 'ss_quantity) (hash-ref sr 'sr_return_quantity)) (hash-ref ss 'ss_sales_price)) (* (hash-ref ss 'ss_quantity) (hash-ref ss 'ss_sales_price)))))))
(define result (let ([groups (make-hash)])
  (for* ([x t]) (let* ([key (hash-ref x 'ss_customer_sk)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons x bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (list (apply + (for*/list ([v (for*/list ([y (hash-ref g 'items)]) (hash-ref y 'act_sales))]) v)) (hash-ref g 'key)))) (string>? (let ([g a]) (list (apply + (for*/list ([v (for*/list ([y (hash-ref g 'items)]) (hash-ref y 'act_sales))]) v)) (hash-ref g 'key))) (let ([g b]) (list (apply + (for*/list ([v (for*/list ([y (hash-ref g 'items)]) (hash-ref y 'act_sales))]) v)) (hash-ref g 'key))))] [(string? (let ([g b]) (list (apply + (for*/list ([v (for*/list ([y (hash-ref g 'items)]) (hash-ref y 'act_sales))]) v)) (hash-ref g 'key)))) (string>? (let ([g a]) (list (apply + (for*/list ([v (for*/list ([y (hash-ref g 'items)]) (hash-ref y 'act_sales))]) v)) (hash-ref g 'key))) (let ([g b]) (list (apply + (for*/list ([v (for*/list ([y (hash-ref g 'items)]) (hash-ref y 'act_sales))]) v)) (hash-ref g 'key))))] [else (> (let ([g a]) (list (apply + (for*/list ([v (for*/list ([y (hash-ref g 'items)]) (hash-ref y 'act_sales))]) v)) (hash-ref g 'key))) (let ([g b]) (list (apply + (for*/list ([v (for*/list ([y (hash-ref g 'items)]) (hash-ref y 'act_sales))]) v)) (hash-ref g 'key))))]))))
  (for/list ([g _groups]) (hash 'ss_customer_sk (hash-ref g 'key) 'sumsales (apply + (for*/list ([v (for*/list ([y (hash-ref g 'items)]) (hash-ref y 'act_sales))]) v))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'ss_customer_sk 1 'sumsales 40) (hash 'ss_customer_sk 2 'sumsales 60))) (displayln "ok"))
