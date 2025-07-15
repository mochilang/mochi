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

(define web (list (hash 'item "A" 'return_ratio 0.2 'currency_ratio 0.3 'return_rank 1 'currency_rank 1) (hash 'item "B" 'return_ratio 0.5 'currency_ratio 0.6 'return_rank 2 'currency_rank 2)))
(define catalog (list (hash 'item "A" 'return_ratio 0.3 'currency_ratio 0.4 'return_rank 1 'currency_rank 1)))
(define store (list (hash 'item "A" 'return_ratio 0.25 'currency_ratio 0.35 'return_rank 1 'currency_rank 1)))
(define tmp (concat (for*/list ([w web] #:when (and (or (_le (hash-ref w 'return_rank) 10) (_le (hash-ref w 'currency_rank) 10)))) (hash 'channel "web" 'item (hash-ref w 'item) 'return_ratio (hash-ref w 'return_ratio) 'return_rank (hash-ref w 'return_rank) 'currency_rank (hash-ref w 'currency_rank))) (for*/list ([c catalog] #:when (and (or (_le (hash-ref c 'return_rank) 10) (_le (hash-ref c 'currency_rank) 10)))) (hash 'channel "catalog" 'item (hash-ref c 'item) 'return_ratio (hash-ref c 'return_ratio) 'return_rank (hash-ref c 'return_rank) 'currency_rank (hash-ref c 'currency_rank))) (for*/list ([s store] #:when (and (or (_le (hash-ref s 'return_rank) 10) (_le (hash-ref s 'currency_rank) 10)))) (hash 'channel "store" 'item (hash-ref s 'item) 'return_ratio (hash-ref s 'return_ratio) 'return_rank (hash-ref s 'return_rank) 'currency_rank (hash-ref s 'currency_rank)))))
(define result (let ([_items0 (for*/list ([r tmp]) (hash 'r r))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((r (hash-ref a 'r))) (list (hash-ref r 'channel) (hash-ref r 'return_rank) (hash-ref r 'currency_rank) (hash-ref r 'item)))) (string<? (let ((r (hash-ref a 'r))) (list (hash-ref r 'channel) (hash-ref r 'return_rank) (hash-ref r 'currency_rank) (hash-ref r 'item))) (let ((r (hash-ref b 'r))) (list (hash-ref r 'channel) (hash-ref r 'return_rank) (hash-ref r 'currency_rank) (hash-ref r 'item))))] [(string? (let ((r (hash-ref b 'r))) (list (hash-ref r 'channel) (hash-ref r 'return_rank) (hash-ref r 'currency_rank) (hash-ref r 'item)))) (string<? (let ((r (hash-ref a 'r))) (list (hash-ref r 'channel) (hash-ref r 'return_rank) (hash-ref r 'currency_rank) (hash-ref r 'item))) (let ((r (hash-ref b 'r))) (list (hash-ref r 'channel) (hash-ref r 'return_rank) (hash-ref r 'currency_rank) (hash-ref r 'item))))] [else (< (let ((r (hash-ref a 'r))) (list (hash-ref r 'channel) (hash-ref r 'return_rank) (hash-ref r 'currency_rank) (hash-ref r 'item))) (let ((r (hash-ref b 'r))) (list (hash-ref r 'channel) (hash-ref r 'return_rank) (hash-ref r 'currency_rank) (hash-ref r 'item))))]))))
  (for/list ([item _items0]) (let ((r (hash-ref item 'r))) r))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'channel "catalog" 'item "A" 'return_ratio 0.3 'return_rank 1 'currency_rank 1) (hash 'channel "store" 'item "A" 'return_ratio 0.25 'return_rank 1 'currency_rank 1) (hash 'channel "web" 'item "A" 'return_ratio 0.2 'return_rank 1 'currency_rank 1) (hash 'channel "web" 'item "B" 'return_ratio 0.5 'return_rank 2 'currency_rank 2))) (displayln "ok"))
