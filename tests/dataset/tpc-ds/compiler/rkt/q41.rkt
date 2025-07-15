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

(define item (list (hash 'product_name "Blue Shirt" 'manufact_id 100 'manufact 1 'category "Women" 'color "blue" 'units "pack" 'size "M") (hash 'product_name "Red Dress" 'manufact_id 120 'manufact 1 'category "Women" 'color "red" 'units "pack" 'size "M") (hash 'product_name "Pants" 'manufact_id 200 'manufact 2 'category "Men" 'color "black" 'units "pair" 'size "L")))
(define lower 100)
(define result (let ([_items0 (for*/list ([i1 item] #:when (and (and (and (_ge (hash-ref i1 'manufact_id) lower) (_le (hash-ref i1 'manufact_id) (+ lower 40))) (_gt (if (and (hash? (for*/list ([i2 item] #:when (and (and (equal? (hash-ref i2 'manufact) (hash-ref i1 'manufact)) (equal? (hash-ref i2 'category) (hash-ref i1 'category))))) i2)) (hash-has-key? (for*/list ([i2 item] #:when (and (and (equal? (hash-ref i2 'manufact) (hash-ref i1 'manufact)) (equal? (hash-ref i2 'category) (hash-ref i1 'category))))) i2) 'items)) (length (hash-ref (for*/list ([i2 item] #:when (and (and (equal? (hash-ref i2 'manufact) (hash-ref i1 'manufact)) (equal? (hash-ref i2 'category) (hash-ref i1 'category))))) i2) 'items)) (length (for*/list ([i2 item] #:when (and (and (equal? (hash-ref i2 'manufact) (hash-ref i1 'manufact)) (equal? (hash-ref i2 'category) (hash-ref i1 'category))))) i2))) 1)))) (hash 'i1 i1))])
  (set! _items0 (sort _items0 (lambda (a b) (cond [(string? (let ((i1 (hash-ref a 'i1))) (hash-ref i1 'product_name))) (string<? (let ((i1 (hash-ref a 'i1))) (hash-ref i1 'product_name)) (let ((i1 (hash-ref b 'i1))) (hash-ref i1 'product_name)))] [(string? (let ((i1 (hash-ref b 'i1))) (hash-ref i1 'product_name))) (string<? (let ((i1 (hash-ref a 'i1))) (hash-ref i1 'product_name)) (let ((i1 (hash-ref b 'i1))) (hash-ref i1 'product_name)))] [else (< (let ((i1 (hash-ref a 'i1))) (hash-ref i1 'product_name)) (let ((i1 (hash-ref b 'i1))) (hash-ref i1 'product_name)))]))))
  (for/list ([item _items0]) (let ((i1 (hash-ref item 'i1))) (hash-ref i1 'product_name)))))
(displayln (jsexpr->string (_json-fix result)))
(when (equal? result '("Blue Shirt" "Red Dress")) (displayln "ok"))
