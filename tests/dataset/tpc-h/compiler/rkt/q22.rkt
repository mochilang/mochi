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

(define customer (list (hash 'c_custkey 1 'c_phone "13-123-4567" 'c_acctbal 600) (hash 'c_custkey 2 'c_phone "31-456-7890" 'c_acctbal 100) (hash 'c_custkey 3 'c_phone "30-000-0000" 'c_acctbal 700)))
(define orders (list (hash 'o_orderkey 10 'o_custkey 2)))
(define valid_codes '("13" "31" "23" "29" "30" "18" "17"))
(define avg_balance (let ([xs (for*/list ([c customer] #:when (and (and (_gt (hash-ref c 'c_acctbal) 0) (cond [(string? valid_codes) (regexp-match? (regexp (substring (hash-ref c 'c_phone) 0 2)) valid_codes)] [(hash? valid_codes) (hash-has-key? valid_codes (substring (hash-ref c 'c_phone) 0 2))] [else (member (substring (hash-ref c 'c_phone) 0 2) valid_codes)])))) (hash-ref c 'c_acctbal))] [n (length (for*/list ([c customer] #:when (and (and (_gt (hash-ref c 'c_acctbal) 0) (cond [(string? valid_codes) (regexp-match? (regexp (substring (hash-ref c 'c_phone) 0 2)) valid_codes)] [(hash? valid_codes) (hash-has-key? valid_codes (substring (hash-ref c 'c_phone) 0 2))] [else (member (substring (hash-ref c 'c_phone) 0 2) valid_codes)])))) (hash-ref c 'c_acctbal)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))))
(define eligible_customers (for*/list ([c customer] #:when (and (and (and (cond [(string? valid_codes) (regexp-match? (regexp (substring (hash-ref c 'c_phone) 0 2)) valid_codes)] [(hash? valid_codes) (hash-has-key? valid_codes (substring (hash-ref c 'c_phone) 0 2))] [else (member (substring (hash-ref c 'c_phone) 0 2) valid_codes)]) (_gt (hash-ref c 'c_acctbal) avg_balance)) (not (not (null? (for*/list ([o orders] #:when (and (equal? (hash-ref o 'o_custkey) (hash-ref c 'c_custkey)))) o))))))) (hash 'cntrycode (substring (hash-ref c 'c_phone) 0 2) 'c_acctbal (hash-ref c 'c_acctbal))))
(define groups (let ([groups (make-hash)])
  (for* ([c eligible_customers]) (let* ([key (hash-ref c 'cntrycode)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons c bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) g)))
(define tmp '())
(for ([g (if (hash? groups) (hash-keys groups) groups)])
(define total (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'c_acctbal))]) v)))
(define row (hash 'cntrycode (hash-ref g 'key) 'numcust (if (and (hash? g) (hash-has-key? g 'items)) (length (hash-ref g 'items)) (length g)) 'totacctbal total))
(set! tmp (append tmp (list row)))
)
(define result (let ([_items0 (for*/list ([r tmp]) (hash 'r r))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((r (hash-ref a 'r))) (hash-ref r 'cntrycode))) (string<? (let ((r (hash-ref a 'r))) (hash-ref r 'cntrycode)) (let ((r (hash-ref b 'r))) (hash-ref r 'cntrycode)))] [(string? (let ((r (hash-ref b 'r))) (hash-ref r 'cntrycode))) (string<? (let ((r (hash-ref a 'r))) (hash-ref r 'cntrycode)) (let ((r (hash-ref b 'r))) (hash-ref r 'cntrycode)))] [else (< (let ((r (hash-ref a 'r))) (hash-ref r 'cntrycode)) (let ((r (hash-ref b 'r))) (hash-ref r 'cntrycode)))]))))
  (for/list ([item _items0]) (let ((r (hash-ref item 'r))) r))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'cntrycode "13" 'numcust 1 'totacctbal 600) (hash 'cntrycode "30" 'numcust 1 'totacctbal 700))) (displayln "ok"))
