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

(define company_type (list (hash 'id 1 'kind "production companies") (hash 'id 2 'kind "distributors")))
(define info_type (list (hash 'id 10 'info "top 250 rank") (hash 'id 20 'info "bottom 10 rank")))
(define title (list (hash 'id 100 'title "Good Movie" 'production_year 1995) (hash 'id 200 'title "Bad Movie" 'production_year 2000)))
(define movie_companies (list (hash 'movie_id 100 'company_type_id 1 'note "ACME (co-production)") (hash 'movie_id 200 'company_type_id 1 'note "MGM (as Metro-Goldwyn-Mayer Pictures)")))
(define movie_info_idx (list (hash 'movie_id 100 'info_type_id 10) (hash 'movie_id 200 'info_type_id 20)))
(define filtered (for*/list ([ct company_type] [mc movie_companies] [t title] [mi movie_info_idx] [it info_type] #:when (and (equal? (hash-ref ct 'id) (hash-ref mc 'company_type_id)) (equal? (hash-ref t 'id) (hash-ref mc 'movie_id)) (equal? (hash-ref mi 'movie_id) (hash-ref t 'id)) (equal? (hash-ref it 'id) (hash-ref mi 'info_type_id)) (and (and (and (string=? (hash-ref ct 'kind) "production companies") (string=? (hash-ref it 'info) "top 250 rank")) (not (regexp-match? (regexp "(as Metro-Goldwyn-Mayer Pictures)") (hash-ref mc 'note)))) (or (regexp-match? (regexp "(co-production)") (hash-ref mc 'note)) (regexp-match? (regexp "(presents)") (hash-ref mc 'note)))))) (hash 'note (hash-ref mc 'note) 'title (hash-ref t 'title) 'year (hash-ref t 'production_year))))
(define result (hash 'production_note (_min (for*/list ([r filtered]) (hash-ref r 'note))) 'movie_title (_min (for*/list ([r filtered]) (hash-ref r 'title))) 'movie_year (_min (for*/list ([r filtered]) (hash-ref r 'year)))))
(displayln (jsexpr->string (list result)))
(when (equal? result (hash 'production_note "ACME (co-production)" 'movie_title "Good Movie" 'movie_year 1995)) (displayln "ok"))
