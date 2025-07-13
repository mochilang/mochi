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

(define cast_info (list (hash 'movie_id 1 'person_id 1 'note "(writer)") (hash 'movie_id 2 'person_id 2 'note "(story)") (hash 'movie_id 3 'person_id 3 'note "(writer)")))
(define company_name (list (hash 'id 1 'name "Lionsgate Pictures") (hash 'id 2 'name "Other Studio")))
(define info_type (list (hash 'id 10 'info "genres") (hash 'id 20 'info "votes")))
(define keyword (list (hash 'id 100 'keyword "murder") (hash 'id 200 'keyword "comedy")))
(define movie_companies (list (hash 'movie_id 1 'company_id 1) (hash 'movie_id 2 'company_id 1) (hash 'movie_id 3 'company_id 2)))
(define movie_info (list (hash 'movie_id 1 'info_type_id 10 'info "Horror") (hash 'movie_id 2 'info_type_id 10 'info "Thriller") (hash 'movie_id 3 'info_type_id 10 'info "Comedy")))
(define movie_info_idx (list (hash 'movie_id 1 'info_type_id 20 'info 1000) (hash 'movie_id 2 'info_type_id 20 'info 800) (hash 'movie_id 3 'info_type_id 20 'info 500)))
(define movie_keyword (list (hash 'movie_id 1 'keyword_id 100) (hash 'movie_id 2 'keyword_id 100) (hash 'movie_id 3 'keyword_id 200)))
(define name (list (hash 'id 1 'name "Arthur" 'gender "m") (hash 'id 2 'name "Bob" 'gender "m") (hash 'id 3 'name "Carla" 'gender "f")))
(define title (list (hash 'id 1 'title "Alpha Horror") (hash 'id 2 'title "Beta Blood") (hash 'id 3 'title "Gamma Comedy")))
(define matches (for*/list ([ci cast_info] [n name] [t title] [mi movie_info] [mi_idx movie_info_idx] [mk movie_keyword] [k keyword] [mc movie_companies] [cn company_name] [it1 info_type] [it2 info_type] #:when (and (equal? (hash-ref n 'id) (hash-ref ci 'person_id)) (equal? (hash-ref t 'id) (hash-ref ci 'movie_id)) (equal? (hash-ref mi 'movie_id) (hash-ref t 'id)) (equal? (hash-ref mi_idx 'movie_id) (hash-ref t 'id)) (equal? (hash-ref mk 'movie_id) (hash-ref t 'id)) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id)) (equal? (hash-ref mc 'movie_id) (hash-ref t 'id)) (equal? (hash-ref cn 'id) (hash-ref mc 'company_id)) (equal? (hash-ref it1 'id) (hash-ref mi 'info_type_id)) (equal? (hash-ref it2 'id) (hash-ref mi_idx 'info_type_id)) (and (and (and (and (and (and (cond [(string? '("(writer)" "(head writer)" "(written by)" "(story)" "(story editor)")) (regexp-match? (regexp (hash-ref ci 'note)) '("(writer)" "(head writer)" "(written by)" "(story)" "(story editor)"))] [(hash? '("(writer)" "(head writer)" "(written by)" "(story)" "(story editor)")) (hash-has-key? '("(writer)" "(head writer)" "(written by)" "(story)" "(story editor)") (hash-ref ci 'note))] [else (member (hash-ref ci 'note) '("(writer)" "(head writer)" "(written by)" "(story)" "(story editor)"))]) (regexp-match? (regexp (string-append "^" (regexp-quote "Lionsgate"))) (hash-ref cn 'name))) (string=? (hash-ref it1 'info) "genres")) (string=? (hash-ref it2 'info) "votes")) (cond [(string? '("murder" "violence" "blood" "gore" "death" "female-nudity" "hospital")) (regexp-match? (regexp (hash-ref k 'keyword)) '("murder" "violence" "blood" "gore" "death" "female-nudity" "hospital"))] [(hash? '("murder" "violence" "blood" "gore" "death" "female-nudity" "hospital")) (hash-has-key? '("murder" "violence" "blood" "gore" "death" "female-nudity" "hospital") (hash-ref k 'keyword))] [else (member (hash-ref k 'keyword) '("murder" "violence" "blood" "gore" "death" "female-nudity" "hospital"))])) (cond [(string? '("Horror" "Thriller")) (regexp-match? (regexp (hash-ref mi 'info)) '("Horror" "Thriller"))] [(hash? '("Horror" "Thriller")) (hash-has-key? '("Horror" "Thriller") (hash-ref mi 'info))] [else (member (hash-ref mi 'info) '("Horror" "Thriller"))])) (string=? (hash-ref n 'gender) "m")))) (hash 'movie_budget (hash-ref mi 'info) 'movie_votes (hash-ref mi_idx 'info) 'writer (hash-ref n 'name) 'violent_liongate_movie (hash-ref t 'title))))
(define result (list (hash 'movie_budget (_min (for*/list ([r matches]) (hash-ref r 'movie_budget))) 'movie_votes (_min (for*/list ([r matches]) (hash-ref r 'movie_votes))) 'writer (_min (for*/list ([r matches]) (hash-ref r 'writer))) 'violent_liongate_movie (_min (for*/list ([r matches]) (hash-ref r 'violent_liongate_movie))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'movie_budget "Horror" 'movie_votes 800 'writer "Arthur" 'violent_liongate_movie "Alpha Horror"))) (displayln "ok"))
