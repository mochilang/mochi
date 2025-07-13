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

(define company_name (list (hash 'id 1 'name "US Studio" 'country_code "[us]") (hash 'id 2 'name "GB Studio" 'country_code "[gb]")))
(define info_type (list (hash 'id 1 'info "rating") (hash 'id 2 'info "other")))
(define kind_type (list (hash 'id 1 'kind "tv series") (hash 'id 2 'kind "movie")))
(define link_type (list (hash 'id 1 'link "follows") (hash 'id 2 'link "remake of")))
(define movie_companies (list (hash 'movie_id 10 'company_id 1) (hash 'movie_id 20 'company_id 2)))
(define movie_info_idx (list (hash 'movie_id 10 'info_type_id 1 'info "7.0") (hash 'movie_id 20 'info_type_id 1 'info "2.5")))
(define movie_link (list (hash 'movie_id 10 'linked_movie_id 20 'link_type_id 1)))
(define title (list (hash 'id 10 'title "Series A" 'kind_id 1 'production_year 2004) (hash 'id 20 'title "Series B" 'kind_id 1 'production_year 2006)))
(define rows (for*/list ([cn1 company_name] [mc1 movie_companies] [t1 title] [mi_idx1 movie_info_idx] [it1 info_type] [kt1 kind_type] [ml movie_link] [t2 title] [mi_idx2 movie_info_idx] [it2 info_type] [kt2 kind_type] [mc2 movie_companies] [cn2 company_name] [lt link_type] #:when (and (equal? (hash-ref cn1 'id) (hash-ref mc1 'company_id)) (equal? (hash-ref t1 'id) (hash-ref mc1 'movie_id)) (equal? (hash-ref mi_idx1 'movie_id) (hash-ref t1 'id)) (equal? (hash-ref it1 'id) (hash-ref mi_idx1 'info_type_id)) (equal? (hash-ref kt1 'id) (hash-ref t1 'kind_id)) (equal? (hash-ref ml 'movie_id) (hash-ref t1 'id)) (equal? (hash-ref t2 'id) (hash-ref ml 'linked_movie_id)) (equal? (hash-ref mi_idx2 'movie_id) (hash-ref t2 'id)) (equal? (hash-ref it2 'id) (hash-ref mi_idx2 'info_type_id)) (equal? (hash-ref kt2 'id) (hash-ref t2 'kind_id)) (equal? (hash-ref mc2 'movie_id) (hash-ref t2 'id)) (equal? (hash-ref cn2 'id) (hash-ref mc2 'company_id)) (equal? (hash-ref lt 'id) (hash-ref ml 'link_type_id)) (and (and (and (and (and (and (and (and (string=? (hash-ref cn1 'country_code) "[us]") (string=? (hash-ref it1 'info) "rating")) (string=? (hash-ref it2 'info) "rating")) (string=? (hash-ref kt1 'kind) "tv series")) (string=? (hash-ref kt2 'kind) "tv series")) (or (or (string=? (hash-ref lt 'link) "sequel") (string=? (hash-ref lt 'link) "follows")) (string=? (hash-ref lt 'link) "followed by"))) (string<? (hash-ref mi_idx2 'info) "3.0")) (>= (hash-ref t2 'production_year) 2005)) (<= (hash-ref t2 'production_year) 2008)))) (hash 'first_company (hash-ref cn1 'name) 'second_company (hash-ref cn2 'name) 'first_rating (hash-ref mi_idx1 'info) 'second_rating (hash-ref mi_idx2 'info) 'first_movie (hash-ref t1 'title) 'second_movie (hash-ref t2 'title))))
(define result (list (hash 'first_company (_min (for*/list ([r rows]) (hash-ref r 'first_company))) 'second_company (_min (for*/list ([r rows]) (hash-ref r 'second_company))) 'first_rating (_min (for*/list ([r rows]) (hash-ref r 'first_rating))) 'second_rating (_min (for*/list ([r rows]) (hash-ref r 'second_rating))) 'first_movie (_min (for*/list ([r rows]) (hash-ref r 'first_movie))) 'second_movie (_min (for*/list ([r rows]) (hash-ref r 'second_movie))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'first_company "US Studio" 'second_company "GB Studio" 'first_rating "7.0" 'second_rating "2.5" 'first_movie "Series A" 'second_movie "Series B"))) (displayln "ok"))
