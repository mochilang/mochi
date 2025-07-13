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

(define company_name (list (hash 'id 1 'name "Euro Films" 'country_code "[de]") (hash 'id 2 'name "US Films" 'country_code "[us]")))
(define company_type (list (hash 'id 1 'kind "production")))
(define info_type (list (hash 'id 10 'info "countries") (hash 'id 20 'info "rating")))
(define keyword (list (hash 'id 1 'keyword "murder") (hash 'id 2 'keyword "comedy")))
(define kind_type (list (hash 'id 100 'kind "movie") (hash 'id 200 'kind "episode")))
(define movie_companies (list (hash 'movie_id 10 'company_id 1 'company_type_id 1 'note "release (2009) (worldwide)") (hash 'movie_id 20 'company_id 2 'company_type_id 1 'note "release (2007) (USA)")))
(define movie_info (list (hash 'movie_id 10 'info_type_id 10 'info "Germany") (hash 'movie_id 20 'info_type_id 10 'info "USA")))
(define movie_info_idx (list (hash 'movie_id 10 'info_type_id 20 'info 6.5) (hash 'movie_id 20 'info_type_id 20 'info 7.8)))
(define movie_keyword (list (hash 'movie_id 10 'keyword_id 1) (hash 'movie_id 20 'keyword_id 2)))
(define title (list (hash 'id 10 'kind_id 100 'production_year 2009 'title "Violent Western") (hash 'id 20 'kind_id 100 'production_year 2007 'title "Old Western")))
(define rows (for*/list ([cn company_name] [mc movie_companies] [ct company_type] [t title] [mk movie_keyword] [k keyword] [mi movie_info] [it1 info_type] [mi_idx movie_info_idx] [it2 info_type] [kt kind_type] #:when (and (equal? (hash-ref cn 'id) (hash-ref mc 'company_id)) (equal? (hash-ref ct 'id) (hash-ref mc 'company_type_id)) (equal? (hash-ref t 'id) (hash-ref mc 'movie_id)) (equal? (hash-ref mk 'movie_id) (hash-ref t 'id)) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id)) (equal? (hash-ref mi 'movie_id) (hash-ref t 'id)) (equal? (hash-ref it1 'id) (hash-ref mi 'info_type_id)) (equal? (hash-ref mi_idx 'movie_id) (hash-ref t 'id)) (equal? (hash-ref it2 'id) (hash-ref mi_idx 'info_type_id)) (equal? (hash-ref kt 'id) (hash-ref t 'kind_id)) (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (not (string=? (hash-ref cn 'country_code) "[us]")) (string=? (hash-ref it1 'info) "countries")) (string=? (hash-ref it2 'info) "rating")) (or (or (or (string=? (hash-ref k 'keyword) "murder") (string=? (hash-ref k 'keyword) "murder-in-title")) (string=? (hash-ref k 'keyword) "blood")) (string=? (hash-ref k 'keyword) "violence"))) (or (string=? (hash-ref kt 'kind) "movie") (string=? (hash-ref kt 'kind) "episode"))) (equal? (regexp-match? (regexp (regexp-quote "(USA)")) (hash-ref mc 'note)) #f)) (regexp-match? (regexp (regexp-quote "(200")) (hash-ref mc 'note))) (or (or (or (string=? (hash-ref mi 'info) "Germany") (string=? (hash-ref mi 'info) "German")) (string=? (hash-ref mi 'info) "USA")) (string=? (hash-ref mi 'info) "American"))) (< (hash-ref mi_idx 'info) 7)) (> (hash-ref t 'production_year) 2008)) (equal? (hash-ref kt 'id) (hash-ref t 'kind_id))) (equal? (hash-ref t 'id) (hash-ref mi 'movie_id))) (equal? (hash-ref t 'id) (hash-ref mk 'movie_id))) (equal? (hash-ref t 'id) (hash-ref mi_idx 'movie_id))) (equal? (hash-ref t 'id) (hash-ref mc 'movie_id))) (equal? (hash-ref mk 'movie_id) (hash-ref mi 'movie_id))) (equal? (hash-ref mk 'movie_id) (hash-ref mi_idx 'movie_id))) (equal? (hash-ref mk 'movie_id) (hash-ref mc 'movie_id))) (equal? (hash-ref mi 'movie_id) (hash-ref mi_idx 'movie_id))) (equal? (hash-ref mi 'movie_id) (hash-ref mc 'movie_id))) (equal? (hash-ref mc 'movie_id) (hash-ref mi_idx 'movie_id))) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id))) (equal? (hash-ref it1 'id) (hash-ref mi 'info_type_id))) (equal? (hash-ref it2 'id) (hash-ref mi_idx 'info_type_id))) (equal? (hash-ref ct 'id) (hash-ref mc 'company_type_id))) (equal? (hash-ref cn 'id) (hash-ref mc 'company_id))))) (hash 'company (hash-ref cn 'name) 'rating (hash-ref mi_idx 'info) 'title (hash-ref t 'title))))
(define result (list (hash 'movie_company (_min (for*/list ([r rows]) (hash-ref r 'company))) 'rating (_min (for*/list ([r rows]) (hash-ref r 'rating))) 'western_violent_movie (_min (for*/list ([r rows]) (hash-ref r 'title))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'movie_company "Euro Films" 'rating 6.5 'western_violent_movie "Violent Western"))) (displayln "ok"))
