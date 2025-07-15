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

(define catalog_sales (list (hash 'item 1 'call 1 'date 1 'price 20) (hash 'item 1 'call 1 'date 2 'price 20) (hash 'item 1 'call 1 'date 3 'price 40)))
(define item (list (hash 'i_item_sk 1 'i_category "A" 'i_brand "B")))
(define call_center (list (hash 'cc_call_center_sk 1 'cc_name "C1")))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2000 'd_moy 11) (hash 'd_date_sk 2 'd_year 2000 'd_moy 12) (hash 'd_date_sk 3 'd_year 2001 'd_moy 1)))
(define (abs x)
  (let/ec return
(if (_ge x 0)
  (begin
(return x)
  )
  (void)
)
(return (- x))
  ))
(define grouped (let ([groups (make-hash)])
  (for* ([cs catalog_sales] [i item] [d date_dim] [cc call_center] #:when (and (equal? (hash-ref cs 'item) (hash-ref i 'i_item_sk)) (equal? (hash-ref cs 'date) (hash-ref d 'd_date_sk)) (equal? (hash-ref cs 'call) (hash-ref cc 'cc_call_center_sk)))) (let* ([key (hash 'cat (hash-ref i 'i_category) 'call (hash-ref cc 'cc_name) 'year (hash-ref d 'd_year))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'cs cs 'i i 'd d 'cc cc) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'cat (hash-ref (hash-ref g 'key) 'cat) 'call (hash-ref (hash-ref g 'key) 'call) 'year (hash-ref (hash-ref g 'key) 'year) 'sum_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v))))))
(define avg_by_year (let ([groups (make-hash)])
  (for* ([g grouped]) (let* ([key (hash 'cat (hash-ref g 'cat) 'call (hash-ref g 'call))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons g bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([gg _groups]) (hash 'cat (hash-ref (hash-ref gg 'key) 'cat) 'call (hash-ref (hash-ref gg 'key) 'call) 'avg_sales (let ([xs (for*/list ([x (hash-ref gg 'items)]) (hash-ref x 'sum_sales))] [n (length (for*/list ([x (hash-ref gg 'items)]) (hash-ref x 'sum_sales)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))))
(define result (for*/list ([g grouped] [a avg_by_year] #:when (and (and (equal? (hash-ref g 'cat) (hash-ref a 'cat)) (equal? (hash-ref g 'call) (hash-ref a 'call))) (and (and (equal? (hash-ref g 'year) 2001) (_gt (hash-ref a 'avg_sales) 0)) (_gt (/ (abs (- (hash-ref g 'sum_sales) (hash-ref a 'avg_sales))) (hash-ref a 'avg_sales)) 0.1)))) (hash 'i_category (hash-ref g 'cat) 'sum_sales (hash-ref g 'sum_sales))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result '()) (displayln "ok"))
