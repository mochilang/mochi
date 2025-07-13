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

(define comp_cast_type (list (hash 'id 1 'kind "cast") (hash 'id 2 'kind "complete cast")))
(define char_name (list (hash 'id 1 'name "Tony Stark") (hash 'id 2 'name "Sherlock Holmes")))
(define complete_cast (list (hash 'movie_id 1 'subject_id 1 'status_id 2) (hash 'movie_id 2 'subject_id 1 'status_id 2)))
(define name (list (hash 'id 1 'name "Robert Downey Jr.") (hash 'id 2 'name "Another Actor")))
(define cast_info (list (hash 'movie_id 1 'person_role_id 1 'person_id 1) (hash 'movie_id 2 'person_role_id 2 'person_id 2)))
(define keyword (list (hash 'id 10 'keyword "superhero") (hash 'id 20 'keyword "romance")))
(define movie_keyword (list (hash 'movie_id 1 'keyword_id 10) (hash 'movie_id 2 'keyword_id 20)))
(define kind_type (list (hash 'id 1 'kind "movie")))
(define title (list (hash 'id 1 'kind_id 1 'production_year 2008 'title "Iron Man") (hash 'id 2 'kind_id 1 'production_year 1940 'title "Old Hero")))
(define matches (for*/list ([cc complete_cast] [cct1 comp_cast_type] [cct2 comp_cast_type] [ci cast_info] [chn char_name] [n name] [mk movie_keyword] [k keyword] [t title] [kt kind_type] #:when (and (equal? (hash-ref cct1 'id) (hash-ref cc 'subject_id)) (equal? (hash-ref cct2 'id) (hash-ref cc 'status_id)) (equal? (hash-ref ci 'movie_id) (hash-ref cc 'movie_id)) (equal? (hash-ref chn 'id) (hash-ref ci 'person_role_id)) (equal? (hash-ref n 'id) (hash-ref ci 'person_id)) (equal? (hash-ref mk 'movie_id) (hash-ref cc 'movie_id)) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id)) (equal? (hash-ref t 'id) (hash-ref cc 'movie_id)) (equal? (hash-ref kt 'id) (hash-ref t 'kind_id)) (and (and (and (and (and (and (string=? (hash-ref cct1 'kind) "cast") (regexp-match? (regexp "complete") (hash-ref cct2 'kind))) (not (regexp-match? (regexp "Sherlock") (hash-ref chn 'name)))) (or (regexp-match? (regexp "Tony Stark") (hash-ref chn 'name)) (regexp-match? (regexp "Iron Man") (hash-ref chn 'name)))) (cond [(string? '("superhero" "sequel" "second-part" "marvel-comics" "based-on-comic" "tv-special" "fight" "violence")) (regexp-match? (regexp (hash-ref k 'keyword)) '("superhero" "sequel" "second-part" "marvel-comics" "based-on-comic" "tv-special" "fight" "violence"))] [(hash? '("superhero" "sequel" "second-part" "marvel-comics" "based-on-comic" "tv-special" "fight" "violence")) (hash-has-key? '("superhero" "sequel" "second-part" "marvel-comics" "based-on-comic" "tv-special" "fight" "violence") (hash-ref k 'keyword))] [else (member (hash-ref k 'keyword) '("superhero" "sequel" "second-part" "marvel-comics" "based-on-comic" "tv-special" "fight" "violence"))])) (string=? (hash-ref kt 'kind) "movie")) (> (hash-ref t 'production_year) 1950)))) (hash-ref t 'title)))
(define result (list (hash 'complete_downey_ironman_movie (_min matches))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'complete_downey_ironman_movie "Iron Man"))) (displayln "ok"))
