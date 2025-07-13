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

(define aka_name (list (hash 'person_id 1 'name "Alpha") (hash 'person_id 2 'name "Beta")))
(define cast_info (list (hash 'person_id 1 'movie_id 101) (hash 'person_id 2 'movie_id 102)))
(define company_name (list (hash 'id 1 'country_code "[us]") (hash 'id 2 'country_code "[de]")))
(define keyword (list (hash 'id 1 'keyword "character-name-in-title") (hash 'id 2 'keyword "other")))
(define movie_companies (list (hash 'movie_id 101 'company_id 1) (hash 'movie_id 102 'company_id 2)))
(define movie_keyword (list (hash 'movie_id 101 'keyword_id 1) (hash 'movie_id 102 'keyword_id 2)))
(define name (list (hash 'id 1) (hash 'id 2)))
(define title (list (hash 'id 101 'title "Hero Bob" 'episode_nr 60) (hash 'id 102 'title "Other Show" 'episode_nr 40)))
(define rows (for*/list ([an aka_name] [n name] [ci cast_info] [t title] [mk movie_keyword] [k keyword] [mc movie_companies] [cn company_name] #:when (and (equal? (hash-ref n 'id) (hash-ref an 'person_id)) (equal? (hash-ref ci 'person_id) (hash-ref n 'id)) (equal? (hash-ref t 'id) (hash-ref ci 'movie_id)) (equal? (hash-ref mk 'movie_id) (hash-ref t 'id)) (equal? (hash-ref k 'id) (hash-ref mk 'keyword_id)) (equal? (hash-ref mc 'movie_id) (hash-ref t 'id)) (equal? (hash-ref cn 'id) (hash-ref mc 'company_id)) (and (and (and (string=? (hash-ref cn 'country_code) "[us]") (string=? (hash-ref k 'keyword) "character-name-in-title")) (>= (hash-ref t 'episode_nr) 50)) (< (hash-ref t 'episode_nr) 100)))) (hash 'pseudonym (hash-ref an 'name) 'series (hash-ref t 'title))))
(define result (list (hash 'cool_actor_pseudonym (_min (for*/list ([r rows]) (hash-ref r 'pseudonym))) 'series_named_after_char (_min (for*/list ([r rows]) (hash-ref r 'series))))))
(displayln (jsexpr->string result))
(when (equal? result (list (hash 'cool_actor_pseudonym "Alpha" 'series_named_after_char "Hero Bob"))) (displayln "ok"))
