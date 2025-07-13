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

(define comp_cast_type (list (hash 'id 1 'kind "crew") (hash 'id 2 'kind "complete+verified") (hash 'id 3 'kind "partial")))
(define complete_cast (list (hash 'movie_id 1 'subject_id 1 'status_id 3) (hash 'movie_id 2 'subject_id 1 'status_id 2)))
(define company_name (list (hash 'id 1 'name "Euro Films Ltd." 'country_code "[gb]") (hash 'id 2 'name "US Studios" 'country_code "[us]")))
(define company_type (list (hash 'id 1) (hash 'id 2)))
(define movie_companies (list (hash 'movie_id 1 'company_id 1 'company_type_id 1 'note "production (2005) (UK)") (hash 'movie_id 2 'company_id 2 'company_type_id 1 'note "production (USA)")))
(define info_type (list (hash 'id 1 'info "countries") (hash 'id 2 'info "rating")))
(define keyword (list (hash 'id 1 'keyword "blood") (hash 'id 2 'keyword "romance")))
(define kind_type (list (hash 'id 1 'kind "movie") (hash 'id 2 'kind "episode")))
(define movie_info (list (hash 'movie_id 1 'info_type_id 1 'info "Germany") (hash 'movie_id 2 'info_type_id 1 'info "USA")))
(define movie_info_idx (list (hash 'movie_id 1 'info_type_id 2 'info 7.2) (hash 'movie_id 2 'info_type_id 2 'info 9)))
(define movie_keyword (list (hash 'movie_id 1 'keyword_id 1) (hash 'movie_id 2 'keyword_id 2)))
(define title (list (hash 'id 1 'kind_id 1 'production_year 2005 'title "Dark Euro Film") (hash 'id 2 'kind_id 1 'production_year 2005 'title "US Film")))
(define allowed_keywords '("murder" "murder-in-title" "blood" "violence"))
(define allowed_countries '("Sweden" "Norway" "Germany" "Denmark" "Swedish" "Danish" "Norwegian" "German" "USA" "American"))
(define matches (for*/list ([cc complete_cast] [cct1 comp_cast_type] [cct2 comp_cast_type] [mc movie_companies] [cn company_name] [ct company_type] [mk movie_keyword] [k keyword] [mi movie_info] [it1 info_type] [mi_idx movie_info_idx] [it2 info_type] [t title] [kt kind_type] #:when (and (equal? (hash-ref cct1 'id) (hash-ref cc 'subject_id)) (equal? (hash-ref cct2 'id) (hash-ref cc 'status_id)) (equal? (hash-ref mc 'movie_id) (hash-ref cc 'movie_id)) (equal? (hash-ref cn 'id) (hash-ref mc 'company_id)) (equal? (hash-ref ct 'id) (hash-ref mc 'company_type_id)) (equal? (hash-ref mk 'movie_id) (hash-ref cc 'movie_id)) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id)) (equal? (hash-ref mi 'movie_id) (hash-ref cc 'movie_id)) (equal? (hash-ref it1 'id) (hash-ref mi 'info_type_id)) (equal? (hash-ref mi_idx 'movie_id) (hash-ref cc 'movie_id)) (equal? (hash-ref it2 'id) (hash-ref mi_idx 'info_type_id)) (equal? (hash-ref t 'id) (hash-ref cc 'movie_id)) (equal? (hash-ref kt 'id) (hash-ref t 'kind_id)) (and (and (and (and (and (and (and (and (and (and (and (string=? (hash-ref cct1 'kind) "crew") (not (string=? (hash-ref cct2 'kind) "complete+verified"))) (not (string=? (hash-ref cn 'country_code) "[us]"))) (string=? (hash-ref it1 'info) "countries")) (string=? (hash-ref it2 'info) "rating")) (cond [(string? allowed_keywords) (regexp-match? (regexp (hash-ref k 'keyword)) allowed_keywords)] [(hash? allowed_keywords) (hash-has-key? allowed_keywords (hash-ref k 'keyword))] [else (member (hash-ref k 'keyword) allowed_keywords)])) (cond [(string? '("movie" "episode")) (regexp-match? (regexp (hash-ref kt 'kind)) '("movie" "episode"))] [(hash? '("movie" "episode")) (hash-has-key? '("movie" "episode") (hash-ref kt 'kind))] [else (member (hash-ref kt 'kind) '("movie" "episode"))])) (equal? (regexp-match? (regexp (regexp-quote "(USA)")) (hash-ref mc 'note)) #f)) (regexp-match? (regexp (regexp-quote "(200")) (hash-ref mc 'note))) (cond [(string? allowed_countries) (regexp-match? (regexp (hash-ref mi 'info)) allowed_countries)] [(hash? allowed_countries) (hash-has-key? allowed_countries (hash-ref mi 'info))] [else (member (hash-ref mi 'info) allowed_countries)])) (< (hash-ref mi_idx 'info) 8.5)) (> (hash-ref t 'production_year) 2000)))) (hash 'company (hash-ref cn 'name) 'rating (hash-ref mi_idx 'info) 'title (hash-ref t 'title))))
(define result (hash 'movie_company (_min (for*/list ([x matches]) (hash-ref x 'company))) 'rating (_min (for*/list ([x matches]) (hash-ref x 'rating))) 'complete_euro_dark_movie (_min (for*/list ([x matches]) (hash-ref x 'title)))))
(displayln (jsexpr->string result))
(when (equal? result (hash 'movie_company "Euro Films Ltd." 'rating 7.2 'complete_euro_dark_movie "Dark Euro Film")) (displayln "ok"))
