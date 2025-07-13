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

(define comp_cast_type (list (hash 'id 1 'kind "cast") (hash 'id 2 'kind "crew") (hash 'id 3 'kind "complete")))
(define complete_cast (list (hash 'movie_id 1 'subject_id 1 'status_id 3) (hash 'movie_id 2 'subject_id 2 'status_id 3)))
(define company_name (list (hash 'id 1 'name "Best Film" 'country_code "[se]") (hash 'id 2 'name "Polish Film" 'country_code "[pl]")))
(define company_type (list (hash 'id 1 'kind "production companies") (hash 'id 2 'kind "other")))
(define keyword (list (hash 'id 1 'keyword "sequel") (hash 'id 2 'keyword "remake")))
(define link_type (list (hash 'id 1 'link "follows") (hash 'id 2 'link "related")))
(define movie_companies (list (hash 'movie_id 1 'company_id 1 'company_type_id 1 'note '()) (hash 'movie_id 2 'company_id 2 'company_type_id 1 'note "extra")))
(define movie_info (list (hash 'movie_id 1 'info "Sweden") (hash 'movie_id 2 'info "USA")))
(define movie_keyword (list (hash 'movie_id 1 'keyword_id 1) (hash 'movie_id 2 'keyword_id 2)))
(define movie_link (list (hash 'movie_id 1 'link_type_id 1) (hash 'movie_id 2 'link_type_id 2)))
(define title (list (hash 'id 1 'production_year 1980 'title "Western Sequel") (hash 'id 2 'production_year 1999 'title "Another Movie")))
(define matches (for*/list ([cc complete_cast] [cct1 comp_cast_type] [cct2 comp_cast_type] [t title] [ml movie_link] [lt link_type] [mk movie_keyword] [k keyword] [mc movie_companies] [ct company_type] [cn company_name] [mi movie_info] #:when (and (equal? (hash-ref cct1 'id) (hash-ref cc 'subject_id)) (equal? (hash-ref cct2 'id) (hash-ref cc 'status_id)) (equal? (hash-ref t 'id) (hash-ref cc 'movie_id)) (equal? (hash-ref ml 'movie_id) (hash-ref t 'id)) (equal? (hash-ref lt 'id) (hash-ref ml 'link_type_id)) (equal? (hash-ref mk 'movie_id) (hash-ref t 'id)) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id)) (equal? (hash-ref mc 'movie_id) (hash-ref t 'id)) (equal? (hash-ref ct 'id) (hash-ref mc 'company_type_id)) (equal? (hash-ref cn 'id) (hash-ref mc 'company_id)) (equal? (hash-ref mi 'movie_id) (hash-ref t 'id)) (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (or (string=? (hash-ref cct1 'kind) "cast") (string=? (hash-ref cct1 'kind) "crew")) (string=? (hash-ref cct2 'kind) "complete")) (not (string=? (hash-ref cn 'country_code) "[pl]"))) (or (regexp-match? (regexp (regexp-quote "Film")) (hash-ref cn 'name)) (regexp-match? (regexp (regexp-quote "Warner")) (hash-ref cn 'name)))) (string=? (hash-ref ct 'kind) "production companies")) (string=? (hash-ref k 'keyword) "sequel")) (regexp-match? (regexp (regexp-quote "follow")) (hash-ref lt 'link))) (equal? (hash-ref mc 'note) '())) (or (or (or (string=? (hash-ref mi 'info) "Sweden") (string=? (hash-ref mi 'info) "Germany")) (string=? (hash-ref mi 'info) "Swedish")) (string=? (hash-ref mi 'info) "German"))) (>= (hash-ref t 'production_year) 1950)) (<= (hash-ref t 'production_year) 2000)) (equal? (hash-ref ml 'movie_id) (hash-ref mk 'movie_id))) (equal? (hash-ref ml 'movie_id) (hash-ref mc 'movie_id))) (equal? (hash-ref mk 'movie_id) (hash-ref mc 'movie_id))) (equal? (hash-ref ml 'movie_id) (hash-ref mi 'movie_id))) (equal? (hash-ref mk 'movie_id) (hash-ref mi 'movie_id))) (equal? (hash-ref mc 'movie_id) (hash-ref mi 'movie_id))) (equal? (hash-ref ml 'movie_id) (hash-ref cc 'movie_id))) (equal? (hash-ref mk 'movie_id) (hash-ref cc 'movie_id))) (equal? (hash-ref mc 'movie_id) (hash-ref cc 'movie_id))) (equal? (hash-ref mi 'movie_id) (hash-ref cc 'movie_id))))) (hash 'company (hash-ref cn 'name) 'link (hash-ref lt 'link) 'title (hash-ref t 'title))))
(define result (hash 'producing_company (_min (for*/list ([x matches]) (hash-ref x 'company))) 'link_type (_min (for*/list ([x matches]) (hash-ref x 'link))) 'complete_western_sequel (_min (for*/list ([x matches]) (hash-ref x 'title)))))
(displayln (jsexpr->string result))
(when (equal? result (hash 'producing_company "Best Film" 'link_type "follows" 'complete_western_sequel "Western Sequel")) (displayln "ok"))
