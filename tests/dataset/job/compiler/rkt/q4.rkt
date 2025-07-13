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

(define info_type (list (hash 'id 1 'info "rating") (hash 'id 2 'info "other")))
(define keyword (list (hash 'id 1 'keyword "great sequel") (hash 'id 2 'keyword "prequel")))
(define title (list (hash 'id 10 'title "Alpha Movie" 'production_year 2006) (hash 'id 20 'title "Beta Film" 'production_year 2007) (hash 'id 30 'title "Old Film" 'production_year 2004)))
(define movie_keyword (list (hash 'movie_id 10 'keyword_id 1) (hash 'movie_id 20 'keyword_id 1) (hash 'movie_id 30 'keyword_id 1)))
(define movie_info_idx (list (hash 'movie_id 10 'info_type_id 1 'info "6.2") (hash 'movie_id 20 'info_type_id 1 'info "7.8") (hash 'movie_id 30 'info_type_id 1 'info "4.5")))
(define rows (for*/list ([it info_type] [mi movie_info_idx] [t title] [mk movie_keyword] [k keyword] #:when (and (equal? (hash-ref it 'id) (hash-ref mi 'info_type_id)) (equal? (hash-ref t 'id) (hash-ref mi 'movie_id)) (equal? (hash-ref mk 'movie_id) (hash-ref t 'id)) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id)) (and (and (and (and (string=? (hash-ref it 'info) "rating") (regexp-match? (regexp "sequel") (hash-ref k 'keyword))) (string>? (hash-ref mi 'info) "5.0")) (> (hash-ref t 'production_year) 2005)) (equal? (hash-ref mk 'movie_id) (hash-ref mi 'movie_id))))) (hash 'rating (hash-ref mi 'info) 'title (hash-ref t 'title))))
(define result (list (hash 'rating (_min (for*/list ([r rows]) (hash-ref r 'rating))) 'movie_title (_min (for*/list ([r rows]) (hash-ref r 'title))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'rating "6.2" 'movie_title "Alpha Movie"))) (displayln "ok"))
