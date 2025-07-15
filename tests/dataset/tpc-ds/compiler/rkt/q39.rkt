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

(define math (hash 'pi 3.141592653589793 'e 2.718281828459045 'sqrt sqrt 'pow expt 'sin sin 'log log))
(define inventory (list (hash 'inv_item_sk 1 'inv_warehouse_sk 1 'inv_date_sk 1 'inv_quantity_on_hand 10) (hash 'inv_item_sk 1 'inv_warehouse_sk 1 'inv_date_sk 2 'inv_quantity_on_hand 10) (hash 'inv_item_sk 1 'inv_warehouse_sk 1 'inv_date_sk 3 'inv_quantity_on_hand 250)))
(define item (list (hash 'i_item_sk 1)))
(define warehouse (list (hash 'w_warehouse_sk 1 'w_warehouse_name "W1")))
(define date_dim (list (hash 'd_date_sk 1 'd_year 2000 'd_moy 1) (hash 'd_date_sk 2 'd_year 2000 'd_moy 2) (hash 'd_date_sk 3 'd_year 2000 'd_moy 3)))
(define monthly (let ([groups (make-hash)])
  (for* ([inv inventory] [d date_dim] [i item] [w warehouse] #:when (and (equal? (hash-ref inv 'inv_date_sk) (hash-ref d 'd_date_sk)) (equal? (hash-ref inv 'inv_item_sk) (hash-ref i 'i_item_sk)) (equal? (hash-ref inv 'inv_warehouse_sk) (hash-ref w 'w_warehouse_sk)) (equal? (hash-ref d 'd_year) 2000))) (let* ([key (hash 'w (hash-ref w 'w_warehouse_sk) 'i (hash-ref i 'i_item_sk) 'month (hash-ref d 'd_moy))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'inv inv 'd d 'i i 'w w) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'w (hash-ref (hash-ref g 'key) 'w) 'i (hash-ref (hash-ref g 'key) 'i) 'qty (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'inv_quantity_on_hand))]) v))))))
(define grouped (hash ))
(for ([m (if (hash? monthly) (hash-keys monthly) monthly)])
(define key (number->string (hash 'w (hash-ref m 'w) 'i (hash-ref m 'i))))
(if (cond [(string? grouped) (regexp-match? (regexp key) grouped)] [(hash? grouped) (hash-has-key? grouped key)] [else (member key grouped)])
  (begin
(define g (cond [(string? grouped) (string-ref grouped key)] [(hash? grouped) (hash-ref grouped key)] [else (list-ref grouped key)]))
(set! grouped (cond [(hash? grouped) (hash-set grouped key (hash 'w (hash-ref g 'w) 'i (hash-ref g 'i) 'qtys (append (hash-ref g 'qtys) (list (hash-ref m 'qty)))))] [else (list-set grouped key (hash 'w (hash-ref g 'w) 'i (hash-ref g 'i) 'qtys (append (hash-ref g 'qtys) (list (hash-ref m 'qty)))))]))
  )
  (begin
(set! grouped (cond [(hash? grouped) (hash-set grouped key (hash 'w (hash-ref m 'w) 'i (hash-ref m 'i) 'qtys (list (hash-ref m 'qty))))] [else (list-set grouped key (hash 'w (hash-ref m 'w) 'i (hash-ref m 'i) 'qtys (list (hash-ref m 'qty))))]))
  )
)
)
(define summary '())
(for ([g (if (hash? (hash-values grouped)) (hash-keys (hash-values grouped)) (hash-values grouped))])
(define mean (let ([xs (hash-ref g 'qtys)] [n (length (hash-ref g 'qtys))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n))))
(define sumsq 0)
(for ([q (if (hash? (hash-ref g 'qtys)) (hash-keys (hash-ref g 'qtys)) (hash-ref g 'qtys))])
(set! sumsq (+ sumsq (* (- q mean) (- q mean))))
)
(define variance (/ sumsq (- (cond [(string? (hash-ref g 'qtys)) (string-length (hash-ref g 'qtys))] [(hash? (hash-ref g 'qtys)) (hash-count (hash-ref g 'qtys))] [else (length (hash-ref g 'qtys))]) 1)))
(define cov (/ ((hash-ref math 'sqrt) variance) mean))
(if (_gt cov 1.5)
  (begin
(set! summary (append summary (list (hash 'w_warehouse_sk (hash-ref g 'w) 'i_item_sk (hash-ref g 'i) 'cov cov))))
  )
  (void)
)
)
(displayln (jsexpr->string (_json-fix summary)))
(when (equal? summary (list (hash 'w_warehouse_sk 1 'i_item_sk 1 'cov 1.539600717839002))) (displayln "ok"))
