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

(define complete_cast (list (hash 'movie_id 1 'status_id 1) (hash 'movie_id 2 'status_id 2)))
(define comp_cast_type (list (hash 'id 1 'kind "complete+verified") (hash 'id 2 'kind "partial")))
(define company_name (list (hash 'id 1 'country_code "[us]") (hash 'id 2 'country_code "[gb]")))
(define company_type (list (hash 'id 1) (hash 'id 2)))
(define info_type (list (hash 'id 1 'info "release dates") (hash 'id 2 'info "other")))
(define keyword (list (hash 'id 1 'keyword "internet") (hash 'id 2 'keyword "other")))
(define kind_type (list (hash 'id 1 'kind "movie") (hash 'id 2 'kind "series")))
(define movie_companies (list (hash 'movie_id 1 'company_id 1 'company_type_id 1) (hash 'movie_id 2 'company_id 2 'company_type_id 2)))
(define movie_info (list (hash 'movie_id 1 'info_type_id 1 'note "internet release" 'info "USA: May 2005") (hash 'movie_id 2 'info_type_id 1 'note "theater" 'info "USA: April 1998")))
(define movie_keyword (list (hash 'movie_id 1 'keyword_id 1) (hash 'movie_id 2 'keyword_id 2)))
(define title (list (hash 'id 1 'kind_id 1 'production_year 2005 'title "Web Movie") (hash 'id 2 'kind_id 1 'production_year 1998 'title "Old Movie")))
(define matches (for*/list ([cc complete_cast] [cct1 comp_cast_type] [t title] [kt kind_type] [mi movie_info] [it1 info_type] [mk movie_keyword] [k keyword] [mc movie_companies] [cn company_name] [ct company_type] #:when (and (equal? (hash-ref cct1 'id) (hash-ref cc 'status_id)) (equal? (hash-ref t 'id) (hash-ref cc 'movie_id)) (equal? (hash-ref kt 'id) (hash-ref t 'kind_id)) (equal? (hash-ref mi 'movie_id) (hash-ref t 'id)) (equal? (hash-ref it1 'id) (hash-ref mi 'info_type_id)) (equal? (hash-ref mk 'movie_id) (hash-ref t 'id)) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id)) (equal? (hash-ref mc 'movie_id) (hash-ref t 'id)) (equal? (hash-ref cn 'id) (hash-ref mc 'company_id)) (equal? (hash-ref ct 'id) (hash-ref mc 'company_type_id)) (and (and (and (and (and (and (string=? (hash-ref cct1 'kind) "complete+verified") (string=? (hash-ref cn 'country_code) "[us]")) (string=? (hash-ref it1 'info) "release dates")) (string=? (hash-ref kt 'kind) "movie")) (regexp-match? (regexp (regexp-quote "internet")) (hash-ref mi 'note))) (and (regexp-match? (regexp (regexp-quote "USA:")) (hash-ref mi 'info)) (or (regexp-match? (regexp (regexp-quote "199")) (hash-ref mi 'info)) (regexp-match? (regexp (regexp-quote "200")) (hash-ref mi 'info))))) (> (hash-ref t 'production_year) 2000)))) (hash 'movie_kind (hash-ref kt 'kind) 'complete_us_internet_movie (hash-ref t 'title))))
(define result (list (hash 'movie_kind (_min (for*/list ([r matches]) (hash-ref r 'movie_kind))) 'complete_us_internet_movie (_min (for*/list ([r matches]) (hash-ref r 'complete_us_internet_movie))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'movie_kind "movie" 'complete_us_internet_movie "Web Movie"))) (displayln "ok"))
