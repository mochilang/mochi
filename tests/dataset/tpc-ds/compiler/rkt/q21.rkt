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

(struct Inventory (inv_item_sk inv_warehouse_sk inv_date_sk inv_quantity_on_hand) #:transparent #:mutable)
(struct Warehouse (w_warehouse_sk w_warehouse_name) #:transparent #:mutable)
(struct Item (i_item_sk i_item_id) #:transparent #:mutable)
(struct DateDim (d_date_sk d_date) #:transparent #:mutable)
(define inventory (list (hash 'inv_item_sk 1 'inv_warehouse_sk 1 'inv_date_sk 1 'inv_quantity_on_hand 30) (hash 'inv_item_sk 1 'inv_warehouse_sk 1 'inv_date_sk 2 'inv_quantity_on_hand 40) (hash 'inv_item_sk 2 'inv_warehouse_sk 2 'inv_date_sk 1 'inv_quantity_on_hand 20) (hash 'inv_item_sk 2 'inv_warehouse_sk 2 'inv_date_sk 2 'inv_quantity_on_hand 20)))
(define warehouse (list (hash 'w_warehouse_sk 1 'w_warehouse_name "Main") (hash 'w_warehouse_sk 2 'w_warehouse_name "Backup")))
(define item (list (hash 'i_item_sk 1 'i_item_id "ITEM1") (hash 'i_item_sk 2 'i_item_id "ITEM2")))
(define date_dim (list (hash 'd_date_sk 1 'd_date "2000-03-01") (hash 'd_date_sk 2 'd_date "2000-03-20")))
(define before (let ([groups (make-hash)])
  (for* ([inv inventory] [d date_dim] #:when (and (equal? (hash-ref inv 'inv_date_sk) (hash-ref d 'd_date_sk)) (string<? (hash-ref d 'd_date) "2000-03-15"))) (let* ([key (hash 'w (hash-ref inv 'inv_warehouse_sk) 'i (hash-ref inv 'inv_item_sk))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'inv inv 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'w (hash-ref (hash-ref g 'key) 'w) 'i (hash-ref (hash-ref g 'key) 'i) 'qty (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'inv_quantity_on_hand))]) v))))))
(define after (let ([groups (make-hash)])
  (for* ([inv inventory] [d date_dim] #:when (and (equal? (hash-ref inv 'inv_date_sk) (hash-ref d 'd_date_sk)) (string>=? (hash-ref d 'd_date) "2000-03-15"))) (let* ([key (hash 'w (hash-ref inv 'inv_warehouse_sk) 'i (hash-ref inv 'inv_item_sk))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'inv inv 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'w (hash-ref (hash-ref g 'key) 'w) 'i (hash-ref (hash-ref g 'key) 'i) 'qty (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref x 'inv_quantity_on_hand))]) v))))))
(define joined (for*/list ([b before] [a after] [w warehouse] [it item] #:when (and (and (equal? (hash-ref b 'w) (hash-ref a 'w)) (equal? (hash-ref b 'i) (hash-ref a 'i))) (equal? (hash-ref w 'w_warehouse_sk) (hash-ref b 'w)) (equal? (hash-ref it 'i_item_sk) (hash-ref b 'i)))) (hash 'w_name (hash-ref w 'w_warehouse_name) 'i_id (hash-ref it 'i_item_id) 'before_qty (hash-ref b 'qty) 'after_qty (hash-ref a 'qty) 'ratio (/ (hash-ref a 'qty) (hash-ref b 'qty)))))
(define result (let ([_items0 (for*/list ([r joined] #:when (and (and (_ge (hash-ref r 'ratio) (/ 2 3)) (_le (hash-ref r 'ratio) (/ 3 2))))) (hash 'r r))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((r (hash-ref a 'r))) (list (hash-ref r 'w_name) (hash-ref r 'i_id)))) (string<? (let ((r (hash-ref a 'r))) (list (hash-ref r 'w_name) (hash-ref r 'i_id))) (let ((r (hash-ref b 'r))) (list (hash-ref r 'w_name) (hash-ref r 'i_id))))] [(string? (let ((r (hash-ref b 'r))) (list (hash-ref r 'w_name) (hash-ref r 'i_id)))) (string<? (let ((r (hash-ref a 'r))) (list (hash-ref r 'w_name) (hash-ref r 'i_id))) (let ((r (hash-ref b 'r))) (list (hash-ref r 'w_name) (hash-ref r 'i_id))))] [else (< (let ((r (hash-ref a 'r))) (list (hash-ref r 'w_name) (hash-ref r 'i_id))) (let ((r (hash-ref b 'r))) (list (hash-ref r 'w_name) (hash-ref r 'i_id))))]))))
  (for/list ([item _items0]) (let ((r (hash-ref item 'r))) (hash 'w_warehouse_name (hash-ref r 'w_name) 'i_item_id (hash-ref r 'i_id) 'inv_before (hash-ref r 'before_qty) 'inv_after (hash-ref r 'after_qty))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'w_warehouse_name "Backup" 'i_item_id "ITEM2" 'inv_before 20 'inv_after 20) (hash 'w_warehouse_name "Main" 'i_item_id "ITEM1" 'inv_before 30 'inv_after 40))) (displayln "ok"))
