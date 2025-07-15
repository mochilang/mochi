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

(define store_sales (list (hash 'item 1 'price 10)))
(define catalog_sales (list (hash 'item 1 'price 20)))
(define web_sales (list (hash 'item 1 'price 30)))
(define ss (let ([groups (make-hash)])
  (for* ([s store_sales]) (let* ([key (hash-ref s 'item)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons s bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'item (hash-ref g 'key) 'total (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v))))))
(define cs (let ([groups (make-hash)])
  (for* ([s catalog_sales]) (let* ([key (hash-ref s 'item)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons s bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'item (hash-ref g 'key) 'total (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v))))))
(define ws (let ([groups (make-hash)])
  (for* ([s web_sales]) (let* ([key (hash-ref s 'item)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons s bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'item (hash-ref g 'key) 'total (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'price))]) v))))))
(define union (concat ss cs ws))
(define result (let ([groups (make-hash)])
  (for* ([u union]) (let* ([key (hash-ref u 'item)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons u bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'i_item_id (hash-ref g 'key) 'total_sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'total))]) v))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'i_item_id 1 'total_sales 60))) (displayln "ok"))
