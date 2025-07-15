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

(define catalog_returns (list (hash 'cust 1 'state "CA" 'amt 40) (hash 'cust 2 'state "CA" 'amt 50) (hash 'cust 3 'state "CA" 'amt 81) (hash 'cust 4 'state "TX" 'amt 30) (hash 'cust 5 'state "TX" 'amt 20)))
(define avg_list (let ([groups (make-hash)])
  (for* ([r catalog_returns]) (let* ([key (hash-ref r 'state)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons r bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'state (hash-ref g 'key) 'avg_amt (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'amt))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'amt)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))))
(define avg_state (first (for*/list ([a avg_list] #:when (and (string=? (hash-ref a 'state) "CA"))) a)))
(define result_list (for*/list ([r catalog_returns] #:when (and (and (string=? (hash-ref r 'state) "CA") (_gt (hash-ref r 'amt) (* (hash-ref avg_state 'avg_amt) 1.2))))) (hash-ref r 'amt)))
(define result (first result_list))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result 81) (displayln "ok"))
