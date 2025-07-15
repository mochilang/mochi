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

(define web_returns (list (hash 'wr_returning_customer_sk 1 'wr_returned_date_sk 1 'wr_return_amt 100 'wr_returning_addr_sk 1) (hash 'wr_returning_customer_sk 2 'wr_returned_date_sk 1 'wr_return_amt 30 'wr_returning_addr_sk 2) (hash 'wr_returning_customer_sk 1 'wr_returned_date_sk 1 'wr_return_amt 50 'wr_returning_addr_sk 1)))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2000)))
(define customer_address (list (hash 'ca_address_sk 1 'ca_state "CA") (hash 'ca_address_sk 2 'ca_state "CA")))
(define customer (list (hash 'c_customer_sk 1 'c_customer_id "C1" 'c_first_name "John" 'c_last_name "Doe" 'c_current_addr_sk 1) (hash 'c_customer_sk 2 'c_customer_id "C2" 'c_first_name "Jane" 'c_last_name "Smith" 'c_current_addr_sk 2)))
(define customer_total_return (let ([groups (make-hash)])
  (for* ([wr web_returns] [d date_dim] [ca customer_address] #:when (and (equal? (hash-ref wr 'wr_returned_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref wr 'wr_returning_addr_sk) (hash-ref ca 'ca_address_sk)) (and (equal? (hash-ref d 'd_year) 2000) (string=? (hash-ref ca 'ca_state) "CA")))) (let* ([key (hash 'cust (hash-ref wr 'wr_returning_customer_sk) 'state (hash-ref ca 'ca_state))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'wr wr 'd d 'ca ca) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'ctr_customer_sk (hash-ref (hash-ref g 'key) 'cust) 'ctr_state (hash-ref (hash-ref g 'key) 'state) 'ctr_total_return (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'wr_return_amt))]) v))))))
(define avg_by_state (let ([groups (make-hash)])
  (for* ([ctr customer_total_return]) (let* ([key (hash-ref ctr 'ctr_state)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons ctr bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'state (hash-ref g 'key) 'avg_return (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ctr_total_return))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ctr_total_return)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))))
(define result (for*/list ([ctr customer_total_return] [avg avg_by_state] [c customer] #:when (and (equal? (hash-ref ctr 'ctr_state) (hash-ref avg 'state)) (equal? (hash-ref ctr 'ctr_customer_sk) (hash-ref c 'c_customer_sk)) (_gt (hash-ref ctr 'ctr_total_return) (* (hash-ref avg 'avg_return) 1.2)))) (hash 'c_customer_id (hash-ref c 'c_customer_id) 'c_first_name (hash-ref c 'c_first_name) 'c_last_name (hash-ref c 'c_last_name) 'ctr_total_return (hash-ref ctr 'ctr_total_return))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'c_customer_id "C1" 'c_first_name "John" 'c_last_name "Doe" 'ctr_total_return 150))) (displayln "ok"))
