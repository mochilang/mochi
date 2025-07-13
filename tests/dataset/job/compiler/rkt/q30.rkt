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

(define comp_cast_type (list (hash 'id 1 'kind "cast") (hash 'id 2 'kind "complete+verified") (hash 'id 3 'kind "crew")))
(define complete_cast (list (hash 'movie_id 1 'subject_id 1 'status_id 2) (hash 'movie_id 2 'subject_id 3 'status_id 2)))
(define cast_info (list (hash 'movie_id 1 'person_id 10 'note "(writer)") (hash 'movie_id 2 'person_id 11 'note "(actor)")))
(define info_type (list (hash 'id 1 'info "genres") (hash 'id 2 'info "votes")))
(define keyword (list (hash 'id 1 'keyword "murder") (hash 'id 2 'keyword "comedy")))
(define movie_info (list (hash 'movie_id 1 'info_type_id 1 'info "Horror") (hash 'movie_id 2 'info_type_id 1 'info "Comedy")))
(define movie_info_idx (list (hash 'movie_id 1 'info_type_id 2 'info 2000) (hash 'movie_id 2 'info_type_id 2 'info 150)))
(define movie_keyword (list (hash 'movie_id 1 'keyword_id 1) (hash 'movie_id 2 'keyword_id 2)))
(define name (list (hash 'id 10 'name "John Writer" 'gender "m") (hash 'id 11 'name "Jane Actor" 'gender "f")))
(define title (list (hash 'id 1 'title "Violent Horror" 'production_year 2005) (hash 'id 2 'title "Old Comedy" 'production_year 1995)))
(define violent_keywords '("murder" "violence" "blood" "gore" "death" "female-nudity" "hospital"))
(define writer_notes '("(writer)" "(head writer)" "(written by)" "(story)" "(story editor)"))
(define matches (for*/list ([cc complete_cast] [cct1 comp_cast_type] [cct2 comp_cast_type] [ci cast_info] [mi movie_info] [mi_idx movie_info_idx] [mk movie_keyword] [it1 info_type] [it2 info_type] [k keyword] [n name] [t title] #:when (and (equal? (hash-ref cct1 'id) (hash-ref cc 'subject_id)) (equal? (hash-ref cct2 'id) (hash-ref cc 'status_id)) (equal? (hash-ref ci 'movie_id) (hash-ref cc 'movie_id)) (equal? (hash-ref mi 'movie_id) (hash-ref cc 'movie_id)) (equal? (hash-ref mi_idx 'movie_id) (hash-ref cc 'movie_id)) (equal? (hash-ref mk 'movie_id) (hash-ref cc 'movie_id)) (equal? (hash-ref it1 'id) (hash-ref mi 'info_type_id)) (equal? (hash-ref it2 'id) (hash-ref mi_idx 'info_type_id)) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id)) (equal? (hash-ref n 'id) (hash-ref ci 'person_id)) (equal? (hash-ref t 'id) (hash-ref cc 'movie_id)) (and (and (and (and (and (and (and (and (cond [(string? '("cast" "crew")) (regexp-match? (regexp (hash-ref cct1 'kind)) '("cast" "crew"))] [(hash? '("cast" "crew")) (hash-has-key? '("cast" "crew") (hash-ref cct1 'kind))] [else (member (hash-ref cct1 'kind) '("cast" "crew"))]) (string=? (hash-ref cct2 'kind) "complete+verified")) (cond [(string? writer_notes) (regexp-match? (regexp (hash-ref ci 'note)) writer_notes)] [(hash? writer_notes) (hash-has-key? writer_notes (hash-ref ci 'note))] [else (member (hash-ref ci 'note) writer_notes)])) (string=? (hash-ref it1 'info) "genres")) (string=? (hash-ref it2 'info) "votes")) (cond [(string? violent_keywords) (regexp-match? (regexp (hash-ref k 'keyword)) violent_keywords)] [(hash? violent_keywords) (hash-has-key? violent_keywords (hash-ref k 'keyword))] [else (member (hash-ref k 'keyword) violent_keywords)])) (cond [(string? '("Horror" "Thriller")) (regexp-match? (regexp (hash-ref mi 'info)) '("Horror" "Thriller"))] [(hash? '("Horror" "Thriller")) (hash-has-key? '("Horror" "Thriller") (hash-ref mi 'info))] [else (member (hash-ref mi 'info) '("Horror" "Thriller"))])) (string=? (hash-ref n 'gender) "m")) (> (hash-ref t 'production_year) 2000)))) (hash 'budget (hash-ref mi 'info) 'votes (hash-ref mi_idx 'info) 'writer (hash-ref n 'name) 'movie (hash-ref t 'title))))
(define result (list (hash 'movie_budget (_min (for*/list ([x matches]) (hash-ref x 'budget))) 'movie_votes (_min (for*/list ([x matches]) (hash-ref x 'votes))) 'writer (_min (for*/list ([x matches]) (hash-ref x 'writer))) 'complete_violent_movie (_min (for*/list ([x matches]) (hash-ref x 'movie))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'movie_budget "Horror" 'movie_votes 2000 'writer "John Writer" 'complete_violent_movie "Violent Horror"))) (displayln "ok"))
