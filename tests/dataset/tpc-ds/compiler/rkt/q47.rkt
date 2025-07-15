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

(define v2 (list (hash 'd_year 2020 'item "A" 'avg_monthly_sales 100 'sum_sales 120) (hash 'd_year 2020 'item "B" 'avg_monthly_sales 80 'sum_sales 70) (hash 'd_year 2019 'item "C" 'avg_monthly_sales 50 'sum_sales 60)))
(define year 2020)
(define orderby "item")
(define (abs x)
  (let/ec return
(if (_ge x 0)
  (begin
(return x)
  )
  (begin
(return (- x))
  )
)
  ))
(define result (let ([_items0 (for*/list ([v v2] #:when (and (and (and (and (_ge (hash-ref v 'd_year) (- year 1)) (_gt (hash-ref v 'avg_monthly_sales) 0)) (_gt (hash-ref v 'sum_sales) (hash-ref v 'avg_monthly_sales))) (_gt (/ (abs (- (hash-ref v 'sum_sales) (hash-ref v 'avg_monthly_sales))) (hash-ref v 'avg_monthly_sales)) 0.1)))) (hash 'v v))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((v (hash-ref a 'v))) (list (- (hash-ref v 'sum_sales) (hash-ref v 'avg_monthly_sales)) (hash-ref v 'item)))) (string<? (let ((v (hash-ref a 'v))) (list (- (hash-ref v 'sum_sales) (hash-ref v 'avg_monthly_sales)) (hash-ref v 'item))) (let ((v (hash-ref b 'v))) (list (- (hash-ref v 'sum_sales) (hash-ref v 'avg_monthly_sales)) (hash-ref v 'item))))] [(string? (let ((v (hash-ref b 'v))) (list (- (hash-ref v 'sum_sales) (hash-ref v 'avg_monthly_sales)) (hash-ref v 'item)))) (string<? (let ((v (hash-ref a 'v))) (list (- (hash-ref v 'sum_sales) (hash-ref v 'avg_monthly_sales)) (hash-ref v 'item))) (let ((v (hash-ref b 'v))) (list (- (hash-ref v 'sum_sales) (hash-ref v 'avg_monthly_sales)) (hash-ref v 'item))))] [else (< (let ((v (hash-ref a 'v))) (list (- (hash-ref v 'sum_sales) (hash-ref v 'avg_monthly_sales)) (hash-ref v 'item))) (let ((v (hash-ref b 'v))) (list (- (hash-ref v 'sum_sales) (hash-ref v 'avg_monthly_sales)) (hash-ref v 'item))))]))))
  (for/list ([item _items0]) (let ((v (hash-ref item 'v))) (hash 'd_year (hash-ref v 'd_year) 'item (hash-ref v 'item) 'avg_monthly_sales (hash-ref v 'avg_monthly_sales) 'sum_sales (hash-ref v 'sum_sales))))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result (list (hash 'd_year 2019 'item "C" 'avg_monthly_sales 50 'sum_sales 60) (hash 'd_year 2020 'item "A" 'avg_monthly_sales 100 'sum_sales 120))) (displayln "ok"))
