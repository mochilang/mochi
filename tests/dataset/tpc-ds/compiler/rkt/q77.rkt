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

(define date_dim (list (hash 'd_date_sk 1 'd_date 1)))
(define store_sales (list (hash 'ss_sold_date_sk 1 's_store_sk 1 'ss_ext_sales_price 100 'ss_net_profit 10)))
(define store_returns (list (hash 'sr_returned_date_sk 1 's_store_sk 1 'sr_return_amt 5 'sr_net_loss 1)))
(define catalog_sales (list (hash 'cs_sold_date_sk 1 'cs_call_center_sk 1 'cs_ext_sales_price 150 'cs_net_profit 15)))
(define catalog_returns (list (hash 'cr_returned_date_sk 1 'cr_call_center_sk 1 'cr_return_amount 7 'cr_net_loss 3)))
(define web_sales (list (hash 'ws_sold_date_sk 1 'ws_web_page_sk 1 'ws_ext_sales_price 200 'ws_net_profit 20)))
(define web_returns (list (hash 'wr_returned_date_sk 1 'wr_web_page_sk 1 'wr_return_amt 10 'wr_net_loss 2)))
(define ss (let ([groups (make-hash)])
  (for* ([ss store_sales] [d date_dim] #:when (and (equal? (hash-ref d 'd_date_sk) (hash-ref ss 'ss_sold_date_sk)))) (let* ([key (hash-ref ss 's_store_sk)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ss ss 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 's_store_sk (hash-ref g 'key) 'sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_ext_sales_price))]) v)) 'profit (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ss) 'ss_net_profit))]) v))))))
(define sr (let ([groups (make-hash)])
  (for* ([sr store_returns] [d date_dim] #:when (and (equal? (hash-ref d 'd_date_sk) (hash-ref sr 'sr_returned_date_sk)))) (let* ([key (hash-ref sr 's_store_sk)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'sr sr 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 's_store_sk (hash-ref g 'key) 'returns (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'sr) 'sr_return_amt))]) v)) 'profit_loss (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'sr) 'sr_net_loss))]) v))))))
(define cs (let ([groups (make-hash)])
  (for* ([cs catalog_sales] [d date_dim] #:when (and (equal? (hash-ref d 'd_date_sk) (hash-ref cs 'cs_sold_date_sk)))) (let* ([key (hash-ref cs 'cs_call_center_sk)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'cs cs 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'cs_call_center_sk (hash-ref g 'key) 'sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'cs) 'cs_ext_sales_price))]) v)) 'profit (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'cs) 'cs_net_profit))]) v))))))
(define cr (let ([groups (make-hash)])
  (for* ([cr catalog_returns] [d date_dim] #:when (and (equal? (hash-ref d 'd_date_sk) (hash-ref cr 'cr_returned_date_sk)))) (let* ([key (hash-ref cr 'cr_call_center_sk)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'cr cr 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'cr_call_center_sk (hash-ref g 'key) 'returns (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'cr) 'cr_return_amount))]) v)) 'profit_loss (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'cr) 'cr_net_loss))]) v))))))
(define ws (let ([groups (make-hash)])
  (for* ([ws web_sales] [d date_dim] #:when (and (equal? (hash-ref d 'd_date_sk) (hash-ref ws 'ws_sold_date_sk)))) (let* ([key (hash-ref ws 'ws_web_page_sk)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'ws ws 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'wp_web_page_sk (hash-ref g 'key) 'sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ws) 'ws_ext_sales_price))]) v)) 'profit (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'ws) 'ws_net_profit))]) v))))))
(define wr (let ([groups (make-hash)])
  (for* ([wr web_returns] [d date_dim] #:when (and (equal? (hash-ref d 'd_date_sk) (hash-ref wr 'wr_returned_date_sk)))) (let* ([key (hash-ref wr 'wr_web_page_sk)] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons (hash 'wr wr 'd d) bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (for/list ([g _groups]) (hash 'wp_web_page_sk (hash-ref g 'key) 'returns (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'wr) 'wr_return_amt))]) v)) 'profit_loss (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'wr) 'wr_net_loss))]) v))))))
(define per_channel (concat (for*/list ([s ss]) (let ((r (findf (lambda (r) (equal? (hash-ref s 's_store_sk) (hash-ref r 's_store_sk))) sr))) (hash 'channel "store channel" 'id (hash-ref s 's_store_sk) 'sales (hash-ref s 'sales) 'returns (if (equal? r '()) 0 (hash-ref r 'returns)) 'profit (- (hash-ref s 'profit) (if (equal? r '()) 0 (hash-ref r 'profit_loss)))))) (for*/list ([c cs] [r cr] #:when (and (equal? (hash-ref c 'cs_call_center_sk) (hash-ref r 'cr_call_center_sk)))) (hash 'channel "catalog channel" 'id (hash-ref c 'cs_call_center_sk) 'sales (hash-ref c 'sales) 'returns (hash-ref r 'returns) 'profit (- (hash-ref c 'profit) (hash-ref r 'profit_loss)))) (for*/list ([w ws]) (let ((r (findf (lambda (r) (equal? (hash-ref w 'wp_web_page_sk) (hash-ref r 'wp_web_page_sk))) wr))) (hash 'channel "web channel" 'id (hash-ref w 'wp_web_page_sk) 'sales (hash-ref w 'sales) 'returns (if (equal? r '()) 0 (hash-ref r 'returns)) 'profit (- (hash-ref w 'profit) (if (equal? r '()) 0 (hash-ref r 'profit_loss))))))))
(define result (let ([groups (make-hash)])
  (for* ([p per_channel]) (let* ([key (hash 'channel (hash-ref p 'channel) 'id (hash-ref p 'id))] [bucket (hash-ref groups key '())]) (hash-set! groups key (cons p bucket))))
  (define _groups (for/list ([k (hash-keys groups)]) (hash 'key k 'items (hash-ref groups k))))
  (set! _groups (sort _groups (lambda (a b) (cond [(string? (let ([g a]) (hash-ref (hash-ref g 'key) 'channel))) (string>? (let ([g a]) (hash-ref (hash-ref g 'key) 'channel)) (let ([g b]) (hash-ref (hash-ref g 'key) 'channel)))] [(string? (let ([g b]) (hash-ref (hash-ref g 'key) 'channel))) (string>? (let ([g a]) (hash-ref (hash-ref g 'key) 'channel)) (let ([g b]) (hash-ref (hash-ref g 'key) 'channel)))] [else (> (let ([g a]) (hash-ref (hash-ref g 'key) 'channel)) (let ([g b]) (hash-ref (hash-ref g 'key) 'channel)))]))))
  (for/list ([g _groups]) (hash 'channel (hash-ref (hash-ref g 'key) 'channel) 'id (hash-ref (hash-ref g 'key) 'id) 'sales (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'p) 'sales))]) v)) 'returns (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'p) 'returns))]) v)) 'profit (apply + (for*/list ([v (for*/list ([x (hash-ref g 'items)]) (hash-ref (hash-ref x 'p) 'profit))]) v))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'channel "catalog channel" 'id 1 'sales 150 'returns 7 'profit 12) (hash 'channel "store channel" 'id 1 'sales 100 'returns 5 'profit 9) (hash 'channel "web channel" 'id 1 'sales 200 'returns 10 'profit 18))) (displayln "ok"))
