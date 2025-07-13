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

(define company_name (list (hash 'id 1 'name "Best Film Co" 'country_code "[us]") (hash 'id 2 'name "Warner Studios" 'country_code "[de]") (hash 'id 3 'name "Polish Films" 'country_code "[pl]")))
(define company_type (list (hash 'id 1 'kind "production companies") (hash 'id 2 'kind "distributors")))
(define keyword (list (hash 'id 1 'keyword "sequel") (hash 'id 2 'keyword "thriller")))
(define link_type (list (hash 'id 1 'link "follow-up") (hash 'id 2 'link "follows from") (hash 'id 3 'link "remake")))
(define movie_companies (list (hash 'movie_id 10 'company_id 1 'company_type_id 1 'note '()) (hash 'movie_id 20 'company_id 2 'company_type_id 1 'note '()) (hash 'movie_id 30 'company_id 3 'company_type_id 1 'note '())))
(define movie_keyword (list (hash 'movie_id 10 'keyword_id 1) (hash 'movie_id 20 'keyword_id 1) (hash 'movie_id 20 'keyword_id 2) (hash 'movie_id 30 'keyword_id 1)))
(define movie_link (list (hash 'movie_id 10 'link_type_id 1) (hash 'movie_id 20 'link_type_id 2) (hash 'movie_id 30 'link_type_id 3)))
(define title (list (hash 'id 10 'production_year 1960 'title "Alpha") (hash 'id 20 'production_year 1970 'title "Beta") (hash 'id 30 'production_year 1985 'title "Polish Movie")))
(define matches (for*/list ([cn company_name] [mc movie_companies] [ct company_type] [t title] [mk movie_keyword] [k keyword] [ml movie_link] [lt link_type] #:when (and (equal? (hash-ref mc 'company_id) (hash-ref cn 'id)) (equal? (hash-ref ct 'id) (hash-ref mc 'company_type_id)) (equal? (hash-ref t 'id) (hash-ref mc 'movie_id)) (equal? (hash-ref mk 'movie_id) (hash-ref t 'id)) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id)) (equal? (hash-ref ml 'movie_id) (hash-ref t 'id)) (equal? (hash-ref lt 'id) (hash-ref ml 'link_type_id)) (and (and (and (and (and (and (and (and (and (and (not (string=? (hash-ref cn 'country_code) "[pl]")) (or (regexp-match? (regexp (regexp-quote "Film")) (hash-ref cn 'name)) (regexp-match? (regexp (regexp-quote "Warner")) (hash-ref cn 'name)))) (string=? (hash-ref ct 'kind) "production companies")) (string=? (hash-ref k 'keyword) "sequel")) (regexp-match? (regexp (regexp-quote "follow")) (hash-ref lt 'link))) (equal? (hash-ref mc 'note) '())) (>= (hash-ref t 'production_year) 1950)) (<= (hash-ref t 'production_year) 2000)) (equal? (hash-ref ml 'movie_id) (hash-ref mk 'movie_id))) (equal? (hash-ref ml 'movie_id) (hash-ref mc 'movie_id))) (equal? (hash-ref mk 'movie_id) (hash-ref mc 'movie_id))))) (hash 'company (hash-ref cn 'name) 'link (hash-ref lt 'link) 'title (hash-ref t 'title))))
(define result (list (hash 'from_company (_min (for*/list ([x matches]) (hash-ref x 'company))) 'movie_link_type (_min (for*/list ([x matches]) (hash-ref x 'link))) 'non_polish_sequel_movie (_min (for*/list ([x matches]) (hash-ref x 'title))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'from_company "Best Film Co" 'movie_link_type "follow-up" 'non_polish_sequel_movie "Alpha"))) (displayln "ok"))
