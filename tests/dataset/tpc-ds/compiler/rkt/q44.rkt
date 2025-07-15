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

(define store_sales (list (hash 'ss_item_sk 1 'ss_store_sk 1 'ss_net_profit 5) (hash 'ss_item_sk 1 'ss_store_sk 1 'ss_net_profit 5) (hash 'ss_item_sk 2 'ss_store_sk 1 'ss_net_profit (- 1))))
(define item (list (hash 'i_item_sk 1 'i_product_name "ItemA") (hash 'i_item_sk 2 'i_product_name "ItemB")))
(define grouped_base (let ([groups (make-hash)])
  (for* ([ss store_sales]) (let* ([key (hash-ref ss 'ss_item_sk)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons ss bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'item_sk (hash-ref g 'key) 'avg_profit (let ([xs (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_net_profit))] [n (length (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'ss_net_profit)))]) (if (= n 0) 0 (/ (for/fold ([s 0.0]) ([v xs]) (+ s (real->double-flonum v))) n)))))))
(define grouped grouped_base)
(define best (first (let ([_items0 (for*/list ([x grouped]) (hash 'x x))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((x (hash-ref a 'x))) (hash-ref x 'avg_profit))) (string>? (let ((x (hash-ref a 'x))) (hash-ref x 'avg_profit)) (let ((x (hash-ref b 'x))) (hash-ref x 'avg_profit)))] [(string? (let ((x (hash-ref b 'x))) (hash-ref x 'avg_profit))) (string>? (let ((x (hash-ref a 'x))) (hash-ref x 'avg_profit)) (let ((x (hash-ref b 'x))) (hash-ref x 'avg_profit)))] [else (> (let ((x (hash-ref a 'x))) (hash-ref x 'avg_profit)) (let ((x (hash-ref b 'x))) (hash-ref x 'avg_profit)))]))))
  (for/list ([item _items0]) (let ((x (hash-ref item 'x))) x)))))
(define worst (first (let ([_items1 (for*/list ([x grouped]) (hash 'x x))])
  (set! _items1 (sort _items1 (lambda (a b) (cond [(string? (let ((x (hash-ref a 'x))) (hash-ref x 'avg_profit))) (string<? (let ((x (hash-ref a 'x))) (hash-ref x 'avg_profit)) (let ((x (hash-ref b 'x))) (hash-ref x 'avg_profit)))] [(string? (let ((x (hash-ref b 'x))) (hash-ref x 'avg_profit))) (string<? (let ((x (hash-ref a 'x))) (hash-ref x 'avg_profit)) (let ((x (hash-ref b 'x))) (hash-ref x 'avg_profit)))] [else (< (let ((x (hash-ref a 'x))) (hash-ref x 'avg_profit)) (let ((x (hash-ref b 'x))) (hash-ref x 'avg_profit)))]))))
  (for/list ([item _items1]) (let ((x (hash-ref item 'x))) x)))))
(define best_name (first (for*/list ([i item] #:when (and (equal? (hash-ref i 'i_item_sk) (hash-ref best 'item_sk)))) (hash-ref i 'i_product_name))))
(define worst_name (first (for*/list ([i item] #:when (and (equal? (hash-ref i 'i_item_sk) (hash-ref worst 'item_sk)))) (hash-ref i 'i_product_name))))
(define result (hash 'best_performing best_name 'worst_performing worst_name))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (hash 'best_performing "ItemA" 'worst_performing "ItemB")) (displayln "ok"))
