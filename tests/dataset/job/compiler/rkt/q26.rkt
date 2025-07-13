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

(define complete_cast (list (hash 'movie_id 1 'subject_id 1 'status_id 2) (hash 'movie_id 2 'subject_id 1 'status_id 2)))
(define comp_cast_type (list (hash 'id 1 'kind "cast") (hash 'id 2 'kind "complete")))
(define char_name (list (hash 'id 1 'name "Spider-Man") (hash 'id 2 'name "Villain")))
(define cast_info (list (hash 'movie_id 1 'person_role_id 1 'person_id 1) (hash 'movie_id 2 'person_role_id 2 'person_id 2)))
(define info_type (list (hash 'id 1 'info "rating")))
(define keyword (list (hash 'id 1 'keyword "superhero") (hash 'id 2 'keyword "comedy")))
(define kind_type (list (hash 'id 1 'kind "movie")))
(define movie_info_idx (list (hash 'movie_id 1 'info_type_id 1 'info 8.5) (hash 'movie_id 2 'info_type_id 1 'info 6.5)))
(define movie_keyword (list (hash 'movie_id 1 'keyword_id 1) (hash 'movie_id 2 'keyword_id 2)))
(define name (list (hash 'id 1 'name "Actor One") (hash 'id 2 'name "Actor Two")))
(define title (list (hash 'id 1 'kind_id 1 'production_year 2005 'title "Hero Movie") (hash 'id 2 'kind_id 1 'production_year 1999 'title "Old Film")))
(define allowed_keywords '("superhero" "marvel-comics" "based-on-comic" "tv-special" "fight" "violence" "magnet" "web" "claw" "laser"))
(define rows (for*/list ([cc complete_cast] [cct1 comp_cast_type] [cct2 comp_cast_type] [ci cast_info] [chn char_name] [n name] [t title] [kt kind_type] [mk movie_keyword] [k keyword] [mi_idx movie_info_idx] [it2 info_type] #:when (and (equal? (hash-ref cct1 'id) (hash-ref cc 'subject_id)) (equal? (hash-ref cct2 'id) (hash-ref cc 'status_id)) (equal? (hash-ref ci 'movie_id) (hash-ref cc 'movie_id)) (equal? (hash-ref chn 'id) (hash-ref ci 'person_role_id)) (equal? (hash-ref n 'id) (hash-ref ci 'person_id)) (equal? (hash-ref t 'id) (hash-ref ci 'movie_id)) (equal? (hash-ref kt 'id) (hash-ref t 'kind_id)) (equal? (hash-ref mk 'movie_id) (hash-ref t 'id)) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id)) (equal? (hash-ref mi_idx 'movie_id) (hash-ref t 'id)) (equal? (hash-ref it2 'id) (hash-ref mi_idx 'info_type_id)) (and (and (and (and (and (and (and (and (string=? (hash-ref cct1 'kind) "cast") (regexp-match? (regexp (regexp-quote "complete")) (hash-ref cct2 'kind))) (not (equal? (hash-ref chn 'name) '()))) (or (regexp-match? (regexp (regexp-quote "man")) (hash-ref chn 'name)) (regexp-match? (regexp (regexp-quote "Man")) (hash-ref chn 'name)))) (string=? (hash-ref it2 'info) "rating")) (cond [(string? allowed_keywords) (regexp-match? (regexp (hash-ref k 'keyword)) allowed_keywords)] [(hash? allowed_keywords) (hash-has-key? allowed_keywords (hash-ref k 'keyword))] [else (member (hash-ref k 'keyword) allowed_keywords)])) (string=? (hash-ref kt 'kind) "movie")) (> (hash-ref mi_idx 'info) 7)) (> (hash-ref t 'production_year) 2000)))) (hash 'character (hash-ref chn 'name) 'rating (hash-ref mi_idx 'info) 'actor (hash-ref n 'name) 'movie (hash-ref t 'title))))
(define result (list (hash 'character_name (_min (for*/list ([r rows]) (hash-ref r 'character))) 'rating (_min (for*/list ([r rows]) (hash-ref r 'rating))) 'playing_actor (_min (for*/list ([r rows]) (hash-ref r 'actor))) 'complete_hero_movie (_min (for*/list ([r rows]) (hash-ref r 'movie))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'character_name "Spider-Man" 'rating 8.5 'playing_actor "Actor One" 'complete_hero_movie "Hero Movie"))) (displayln "ok"))
